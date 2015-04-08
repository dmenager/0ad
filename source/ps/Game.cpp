/* Copyright (C) 2013 Wildfire Games.
 * This file is part of 0 A.D.
 *
 * 0 A.D. is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * 0 A.D. is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with 0 A.D.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "precompiled.h"

#include "Game.h"
#include "SocketClient.h"
#include "graphics/GameView.h"
#include "graphics/LOSTexture.h"
#include "graphics/ParticleManager.h"
#include "graphics/UnitManager.h"
#include "gui/GUIManager.h"
#include "gui/CGUI.h"
#include "lib/config2.h"
#include "lib/timer.h"
#include "network/NetClient.h"
#include "network/NetServer.h"
#include "network/NetTurnManager.h"
#include "ps/CConsole.h"
#include "ps/CLogger.h"
#include "ps/CStr.h"
#include "ps/Loader.h"
#include "ps/LoaderThunks.h"
#include "ps/Overlay.h"
#include "ps/Profile.h"
#include "ps/Replay.h"
#include "ps/World.h"
#include "ps/GameSetup/GameSetup.h"
#include "renderer/Renderer.h"
#include "renderer/TimeManager.h"
#include "renderer/WaterManager.h"
#include "scriptinterface/ScriptInterface.h"
#include "simulation2/Simulation2.h"
#include "simulation2/components/ICmpPlayer.h"
#include "simulation2/components/ICmpPlayerManager.h"
#include "soundmanager/ISoundManager.h"

#include "tools/atlas/GameInterface/GameLoop.h"

#include "i18n/L10n.h"
#include "lib/utf8.h"

#ifndef _Longlong
#define _Longlong long long
#endif // _Longlong
extern bool g_GameRestarted;
extern GameLoopState* g_AtlasGameLoop;

/**
 * Globally accessible pointer to the CGame object.
 **/
CGame *g_Game=NULL;

/**
 * Constructor
 *
 **/
CGame::CGame(bool disableGraphics):
	m_World(new CWorld(this)),
	m_Simulation2(new CSimulation2(&m_World->GetUnitManager(), g_ScriptRuntime, m_World->GetTerrain())),
	m_GameView(disableGraphics ? NULL : new CGameView(this)),
	m_GameStarted(false),
	m_Paused(false),
	m_SimRate(1.0f),
	m_PlayerID(-1),
	m_IsSavedGame(false)
{
	m_ReplayLogger = new CReplayLogger(m_Simulation2->GetScriptInterface());
	// TODO: should use CDummyReplayLogger unless activated by cmd-line arg, perhaps?

	// Need to set the CObjectManager references after various objects have
	// been initialised, so do it here rather than via the initialisers above.
	if (m_GameView)
		m_World->GetUnitManager().SetObjectManager(m_GameView->GetObjectManager());

	m_TurnManager = new CNetLocalTurnManager(*m_Simulation2, GetReplayLogger()); // this will get replaced if we're a net server/client

	m_Simulation2->LoadDefaultScripts();
}

/**
 * Destructor
 *
 **/
CGame::~CGame()
{
	// Again, the in-game call tree is going to be different to the main menu one.
	if (CProfileManager::IsInitialised())
		g_Profiler.StructuralReset();

	delete m_TurnManager;
	delete m_GameView;
	delete m_Simulation2;
	delete m_World;
	delete m_ReplayLogger;
}

void CGame::SetTurnManager(CNetTurnManager* turnManager)
{
	if (m_TurnManager)
		delete m_TurnManager;

	m_TurnManager = turnManager;

	if (m_TurnManager)
		m_TurnManager->SetPlayerID(m_PlayerID);
}


/**
 * Initializes the game with the set of attributes provided.
 * Makes calls to initialize the game view, world, and simulation objects.
 * Calls are made to facilitate progress reporting of the initialization.
 **/
void CGame::RegisterInit(const JS::HandleValue attribs, const std::string& savedState)
{
	JSContext* cx = m_Simulation2->GetScriptInterface().GetContext();
	JSAutoRequest rq(cx);

	m_InitialSavedState = savedState;
	m_IsSavedGame = !savedState.empty();

	CScriptValRooted tmpAttribs(cx, attribs);
	m_Simulation2->SetInitAttributes(tmpAttribs);

	std::string mapType;
	m_Simulation2->GetScriptInterface().GetProperty(attribs, "mapType", mapType);

	float speed;
	if (m_Simulation2->GetScriptInterface().HasProperty(attribs, "gameSpeed") && m_Simulation2->GetScriptInterface().GetProperty(attribs, "gameSpeed", speed))
		SetSimRate(speed);

	LDR_BeginRegistering();

	RegMemFun(m_Simulation2, &CSimulation2::ProgressiveLoad, L"Simulation init", 1000);

	// RC, 040804 - GameView needs to be initialized before World, otherwise GameView initialization
	// overwrites anything stored in the map file that gets loaded by CWorld::Initialize with default
	// values.  At the minute, it's just lighting settings, but could be extended to store camera position.
	// Storing lighting settings in the game view seems a little odd, but it's no big deal; maybe move it at
	// some point to be stored in the world object?
	if (m_GameView)
		m_GameView->RegisterInit();

	if (mapType == "random")
	{
		// Load random map attributes
		std::wstring scriptFile;
		CScriptValRooted settings;

		m_Simulation2->GetScriptInterface().GetProperty(attribs, "script", scriptFile);
		m_Simulation2->GetScriptInterface().GetProperty(attribs, "settings", settings);

		m_World->RegisterInitRMS(scriptFile, settings, m_PlayerID);
	}
	else
	{
		std::wstring mapFile;
		m_Simulation2->GetScriptInterface().GetProperty(attribs, "map", mapFile);
		CScriptValRooted settings;
		m_Simulation2->GetScriptInterface().GetProperty(attribs, "settings", settings);

		m_World->RegisterInit(mapFile, settings, m_PlayerID);
	}
	if (m_GameView)
		RegMemFun(g_Renderer.GetSingletonPtr()->GetWaterManager(), &WaterManager::LoadWaterTextures, L"LoadWaterTextures", 80);

	if (m_IsSavedGame)
		RegMemFun(this, &CGame::LoadInitialState, L"Loading game", 1000);

	LDR_EndRegistering();
}

int CGame::LoadInitialState()
{
	ENSURE(m_IsSavedGame);
	ENSURE(!m_InitialSavedState.empty());

	std::string state;
	m_InitialSavedState.swap(state); // deletes the original to save a bit of memory

	std::stringstream stream(state);

	bool ok = m_Simulation2->DeserializeState(stream);
	if (!ok)
	{
		CancelLoad(L"Failed to load saved game state. It might have been\nsaved with an incompatible version of the game.");
		return 0;
	}

	return 0;
}

//DC
int prevTime = 0;
int currTime = 0;
time_t now;
tm *ltm;

/**
 * Game initialization has been completed. Set game started flag and start the session.
 *
 * @return PSRETURN 0
 **/
PSRETURN CGame::ReallyStartGame()
{
	JSContext* cx = m_Simulation2->GetScriptInterface().GetContext();
	JSAutoRequest rq(cx);

	// Call the script function InitGame only for new games, not saved games
	if (!m_IsSavedGame)
	{
		if (!g_AtlasGameLoop->running)
		{
			// We need to replace skirmish "default" entities with real ones.
			// This needs to happen before AI initialization (in InitGame).
			// And we need to flush destroyed entities otherwise the AI
			// gets the wrong game state in the beginning and a bunch of
			// "destroy" messages on turn 0, which just shouldn't happen.
			m_Simulation2->ReplaceSkirmishGlobals();
			m_Simulation2->FlushDestroyedEntities();
		}
		JS::RootedValue settings(cx);
		JS::RootedValue tmpInitAttributes(cx, m_Simulation2->GetInitAttributes().get());
		m_Simulation2->GetScriptInterface().GetProperty(tmpInitAttributes, "settings", &settings);
		m_Simulation2->InitGame(CScriptVal(settings));
	}

	// We need to do an initial Interpolate call to set up all the models etc,
	// because Update might never interpolate (e.g. if the game starts paused)
	// and we could end up rendering before having set up any models (so they'd
	// all be invisible)
	Interpolate(0, 0);

	m_GameStarted=true;

	// Render a frame to begin loading assets
	if (CRenderer::IsInitialised())
		Render();

	// Call the reallyStartGame GUI function, but only if it exists
	if (g_GUI && g_GUI->HasPages())
	{
		JS::RootedValue global(cx, g_GUI->GetActiveGUI()->GetGlobalObject());
		if (g_GUI->GetActiveGUI()->GetScriptInterface()->HasProperty(global, "reallyStartGame"))
			g_GUI->GetActiveGUI()->GetScriptInterface()->CallFunctionVoid(global, "reallyStartGame");
	}

	if (g_NetClient)
		g_NetClient->LoadFinished();

	debug_printf(L"GAME STARTED, ALL INIT COMPLETE\n");

	// The call tree we've built for pregame probably isn't useful in-game.
	if (CProfileManager::IsInitialised())
		g_Profiler.StructuralReset();

	// Mark terrain as modified so the minimap can repaint (is there a cleaner way of handling this?)
	g_GameRestarted = true;

	//get initial State when game begins
	m_Simulation2->addPlayerState();
	prevTime = m_Simulation2->getGameTime();

	return 0;
}

int CGame::GetPlayerID()
{
	return m_PlayerID;
}

void CGame::SetPlayerID(int playerID)
{
	m_PlayerID = playerID;
	if (m_TurnManager)
		m_TurnManager->SetPlayerID(m_PlayerID);
}

int count = 0;
std::string year, month, day, hour, minute, second;

void CGame::StartGame(const CScriptValRooted& attribs1, const std::string& savedState)
{
	JSContext* cx = m_Simulation2->GetScriptInterface().GetContext();
	JSAutoRequest rq(cx);

	JS::RootedValue attribs(cx, attribs1.get()); // TODO: Get Handle parameter directly with SpiderMonkey 31
	m_ReplayLogger->StartGame(&attribs);

	// Generate serial number based on time and date, as well as player. pssmmhhddmmyyyy
	now = time(0);
	ltm = gmtime(&now);
	year = std::to_string((_Longlong) 1900 + ltm->tm_year);
	month = std::to_string ((_Longlong) 1 + ltm->tm_mon);
	day = std::to_string ((_Longlong) ltm->tm_mday);
	hour = std::to_string ((_Longlong) ltm->tm_hour);
	minute = std::to_string ((_Longlong) ltm->tm_min + 1);
	second = std::to_string ((_Longlong) ltm-> tm_sec);

	RegisterInit(attribs, savedState);
}


pthread_t stateThread;
// TODO: doInterpolate is optional because Atlas interpolates explicitly,
// so that it has more control over the update rate. The game might want to
// do the same, and then doInterpolate should be redundant and removed.
bool CGame::Update(const double deltaRealTime, bool doInterpolate)
{
	if (m_Paused)
		return true;

	if (!m_TurnManager)
		return true;
	
	const double deltaSimTime = deltaRealTime * m_SimRate;

	bool ok = true;
	if (deltaSimTime)
	{
		//Update and Record state every 30 seconds
		count++;
		currTime = m_Simulation2->getGameTime();
		pthread_create(&stateThread, NULL, &sendState, NULL);
		
		// To avoid confusing the profiler, we need to trigger the new turn
		// while we're not nested inside any PROFILE blocks
		if (m_TurnManager->WillUpdate(deltaSimTime))
			g_Profiler.Turn();

		// At the normal sim rate, we currently want to render at least one
		// frame per simulation turn, so let maxTurns be 1. But for fast-forward
		// sim rates we want to allow more, so it's not bounded by framerate,
		// so just use the sim rate itself as the number of turns per frame.
		size_t maxTurns = (size_t)m_SimRate;

		if (m_TurnManager->Update(deltaSimTime, maxTurns))
		{
			{
				PROFILE3("gui sim update");
				g_GUI->SendEventToAll("SimulationUpdate");
			}

			GetView()->GetLOSTexture().MakeDirty();
		}

		if (CRenderer::IsInitialised())
			g_Renderer.GetTimeManager().Update(deltaSimTime);
		pthread_join(stateThread, NULL);
	}

	if (doInterpolate)
	{
		m_TurnManager->Interpolate(deltaSimTime, deltaRealTime);

		if ( g_SoundManager )
			g_SoundManager->IdleTask();
	}

	return ok;
}

void* sendState(void* p)
{
	CSimulation2* simulation = g_Game->GetSimulation2();
			
	if ( (currTime - prevTime) >= 30 ) {
			prevTime = currTime;
			LOGMESSAGERENDER(wstring_from_utf8(L10n::Instance().Translate("Send State") + "\n").c_str());
			simulation->addPlayerState();

			std::ofstream myfile;
			std::vector<std::vector<std::vector<int32_t>>> stateTable = simulation->getStateTable();
			std::vector<std::string> playerLabels = simulation->getPlayerLabels();

			int outside = stateTable.size();
			// Players
#ifdef _WIN32
			for(int i = 1; i < stateTable.size(); i++)
			{
				// Add the player to the generated serial number.
				std::string stri;
				stri = std::to_string ((_Longlong) i);
				std::string sn = stri + second + minute + hour + day + month + year;
				std::string buffer_Windows = sn + "\t";
				myfile.open ("C:\\0adtestdata\\" + sn + ".txt");
				int middle = stateTable[i].size();

				//>>>>TODO : Add Good formatting by having a new column every x spaces
				std::string tabs = "\t\t\t";

				myfile << "time" + tabs
					   << "CurrentFood"+tabs+"dCurrentFood"+tabs
					   << "FoodUsed"+tabs+"dFoodUsed"+tabs
					   << "FoodSold"+tabs+"dFoodSold"+tabs
					   << "FoodBought"+tabs+"dFoodBought"+tabs
					   << "FoodGathered"+tabs+"dFoodGathered"+tabs
					   << "CurrentWood"+tabs+"dCurrentWood"+tabs
					   << "WoodUsed"+tabs+"dWoodUsed"+tabs
					   << "WoodSold"+tabs+"dWoodSold"+tabs
					   << "WoodBought"+tabs+"dWoodBought"+tabs
					   << "WoodGathered"+tabs+"dWoodGathered"+tabs
					   << "CurrentMetal"+tabs+"dCurrentMetal"+tabs
					   << "MetalUsed"+tabs+"dMetalUsed"+tabs
					   << "MetalSold"+tabs+"dMetalSold"+tabs
					   << "MetalBought"+tabs+"dMetalBought"+tabs
					   << "MetalGathered"+tabs+"dMetalGathered"+tabs
					   << "CurrentStone"+tabs+"dCurrentStone"+tabs
					   << "StoneUsed"+tabs+"dStoneUsed"+tabs
					   << "StoneSold"+tabs+"dStoneSold"+tabs
					   << "StoneBought"+tabs+"dStoneBought"+tabs
					   << "StoneGathered"+tabs+"dStoneGathered"+tabs
					   << "CurrentInfantry"+tabs+"dCurrentInfantry"+tabs
					   << "InfantryGained"+tabs+"dInfantryGained"+tabs
					   << "InfantryLost"+tabs+"dInfantryLost"+tabs
					   << "InfantryKilled"+tabs+"dInfantryKilled"+tabs
					   << "CurrentCavalry"+tabs+"dCurrentCavalry"+tabs
					   << "CavalryGained"+tabs+"dCavalryGained"+tabs
					   << "CavalryLost"+tabs+"dCavalryLost"+tabs
					   << "CavalryKilled"+tabs+"dCavalryKilled"+tabs
					   << "CurrentSupport"+tabs+"dCurrentSupport"+tabs
					   << "SupportGained"+tabs+"dSupportGained"+tabs
					   << "SupportLost"+tabs+"dSupportLost"+tabs
					   << "SupportKilled"+tabs+"dSupportKilled"+tabs
					   << "CurrentSiege"+tabs+"dCurrentSiege"+tabs
					   << "SiegeGained"+tabs+"dSiegeGained"+tabs
					   << "SiegeLost"+tabs+"dSiegeLost"+tabs
					   << "SiegeKilled"+tabs+"dSiegeKilled"+tabs
					   << "CurrentShips"+tabs+"dCurrentShips"+tabs
					   << "ShipsGained"+tabs+"dShipsGained"+tabs
					   << "ShipsLost"+tabs+"dShipsLost"+tabs
					   << "ShipsKilled"+tabs+"dShipsKilled"+tabs
					   << "CurrentStructures"+tabs+"dCurrentStructures"+tabs
					   << "StructuresGained"+tabs+"dStructuresGained"+tabs
					   << "StructuresLost"+tabs+"dStructuresLost"+tabs
					   << "StructuresDestroyed"+tabs+"dStructuresDestroyed"+tabs
					   << "MilitaryMovementsOccurred"+tabs
					   << "\n";

				for(int j = 0; j < stateTable[i].size(); j++)
				{
					int inner = stateTable[i][j].size();
					for(int k = 0; k < stateTable[i][j].size(); k++)
					{
						myfile << std::to_string( (_Longlong) stateTable[i][j][k] ) << tabs;
						if (j == stateTable[i].size()-1 && i == 1)
							buffer_Windows += std::to_string( (_Longlong) stateTable[i][j][k] ) + "\t";
					}

					//write the user inputed labels
					if( i == 1 )
					{
						myfile << playerLabels[j] << std::endl;
						if (j == stateTable[i].size()-1 && i == 1)
							buffer_Windows = buffer_Windows + playerLabels[j] + "\n";
					}
					else
					{
						myfile << "null\n";
						if (j == stateTable[i].size()-1 && i == 1)
							buffer_Windows += "null\n";
					}

					if (j == stateTable[i].size()-1 && i == 1) {
						char * receive_buffer;
						receive_buffer = new char[BUFLEN];
						receive_buffer = sendtoServer_Windows(buffer_Windows);

						LOGMESSAGERENDER(wstring_from_utf8(L10n::Instance().Translate(receive_buffer) + "\n").c_str());
					}
				}
				myfile.close();
			}
#endif
#ifdef __linux
			for(int i = 1; i < stateTable.size(); i++)
			{
				// Add the player to the generated serial number.
				std::string stri;
				stri = std::to_string ((_Longlong) i);
				std::string sn = stri + second + minute + hour + day + month + year;
				myfile.open ("~/0adtestdata/" + sn + ".txt");

    /*
				// Socket shenanigans.
				int sockfd;
				int portno = SERVER_PORT; //SERVER_PORT in SocketClient.h
				int n;
				char buffer[1024];
				struct sockaddr_in serv_addr;
				struct hostent *server;
				try
				{
						sockfd = socket(AF_INET, SOCK_STREAM, 0);
						server = gethostbyname(SERVER_ADDR); //SERVER_ADDR in SocketClient.h
						bzero((char *) &serv_addr, sizeof(serv_addr));
						serv_addr.sin_port = htons(portno);
						connect(sockfd,(struct sockaddr *) &serv_addr, sizeof(serv_addr));
						bzero(buffer, 1024);
						buffer << sn << "\t";
				}
				catch(...)
                {
                    //do nothing
                }
      */
				int middle = stateTable[i].size();
				myfile << "time\t\t"
					   << "food\t\tdFood\t\t"
					   << "wood\t\tdWood\t\t"
					   << "stone\t\tdStone\t\t"
					   << "metal\t\tdMetal\t\t"
					   << "inf\t\tdInf\t\t"
					   << "wrkr\t\tdWrkr\t\t"
					   << "fmales\t\tdFmales\t\t"
					   << "cvlry\t\tdCvlry\t\t"
					   << "chmp\t\tdChmp\t\t"
					   << "hero\t\tdHero\t\t"
					   << "ships\t\tdShips\t\t"
					   << "house\t\tdHouse\t\t"
					   << "econ\t\tdEcon\t\t"
					   << "outpst\t\tdOutpst\t\t"
					   << "mltry\t\tdMltry\t\t"
					   << "fortr\t\tdFortr\t\t"
					   << "civCnt\t\tdCivCnt\t\t"
					   << "wndr\t\tdWndr\t\t"
					   << "enK\t\tdEnK\t\t"
					   << "enBldD\t\tdEnBldD\t\t"
					   << "unitsL\t\tdUnitsL\t\t"
					   << "bldL\t\tdBldL\t\tLabel\n";

				for(int j = 0; j < stateTable[i].size(); j++)
				{
					int inner = stateTable[i][j].size();

					for(int k = 0; k < stateTable[i][j].size(); k++)
					{
							myfile << std::to_string( (_Longlong) stateTable[i][j][k] ) <<"\t";
							//buffer << std::to_string( (_Longlong) stateTable[i][j][k] ) <<"\t";
					}

					//write the user inputed labels
					if( i == 1 )
					{
						myfile << playerLabels[j] << std::endl;
						//buffer << playerLabels[j] << std::endl;
					}
					else
					{
						myfile << "null\n";
						//buffer << "null\n";
					}

					try
					{
						//n = write(sockfd, buffer, strlen(buffer));
					}
                    catch(...)
                    {
                        //do nothing
                    }
				}
				myfile.close();
			}
#endif
		}
		return NULL;
}

void CGame::Interpolate(float simFrameLength, float realFrameLength)
{
	if (!m_TurnManager)
		return;

	m_TurnManager->Interpolate(simFrameLength, realFrameLength);
}


static CColor BrokenColor(0.3f, 0.3f, 0.3f, 1.0f);

void CGame::CachePlayerColours()
{
	m_PlayerColours.clear();

	CmpPtr<ICmpPlayerManager> cmpPlayerManager(*m_Simulation2, SYSTEM_ENTITY);
	if (!cmpPlayerManager)
		return;

	int numPlayers = cmpPlayerManager->GetNumPlayers();
	m_PlayerColours.resize(numPlayers);

	for (int i = 0; i < numPlayers; ++i)
	{
		CmpPtr<ICmpPlayer> cmpPlayer(*m_Simulation2, cmpPlayerManager->GetPlayerByID(i));
		if (!cmpPlayer)
			m_PlayerColours[i] = BrokenColor;
		else
			m_PlayerColours[i] = cmpPlayer->GetColour();
	}
}


CColor CGame::GetPlayerColour(int player) const
{
	if (player < 0 || player >= (int)m_PlayerColours.size())
		return BrokenColor;

	return m_PlayerColours[player];
}
