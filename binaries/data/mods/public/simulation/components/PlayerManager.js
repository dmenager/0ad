function PlayerManager() {}

PlayerManager.prototype.Schema =
	"<a:component type='system'/><empty/>";

PlayerManager.prototype.Init = function()
{
	this.playerEntities = []; // list of player entity IDs
};

PlayerManager.prototype.AddPlayer = function(ent)
{
	var id = this.playerEntities.length;
	var cmpPlayer = Engine.QueryInterface(ent, IID_Player)
	cmpPlayer.SetPlayerID(id);
	this.playerEntities.push(ent);
	// initialize / update the diplomacy arrays
	var newDiplo = [];
	for (var i = 0; i < id; i++)
	{
		var cmpOtherPlayer = Engine.QueryInterface(this.GetPlayerByID(i), IID_Player);
		cmpOtherPlayer.diplomacy[id] = -1;
		newDiplo[i] = -1;
	}
	newDiplo[id] = 1;
	cmpPlayer.SetDiplomacy(newDiplo);
	
	return id;
};

/**
 * Returns the player entity ID for the given player ID.
 * The player ID must be valid (else there will be an error message).
 */
PlayerManager.prototype.GetPlayerByID = function(id)
{
	if (id in this.playerEntities)
		return this.playerEntities[id];

	// All players at or below ID 0 get gaia-level data. (Observers for example)
	if (id <= 0)
		return this.playerEntities[0];

	var stack = new Error().stack.trimRight().replace(/^/mg, '  '); // indent each line
	warn("GetPlayerByID: no player defined for id '"+id+"'\n"+stack);

	return INVALID_ENTITY;
};

/**
 * returns player 1's units trained
 */
PlayerManager.prototype.GetPlayerUnitsTrained = function()
{

	//for (var i = 0; i < n; ++i)
	//{
		var playerEnt = this.GetPlayerByID(1);
		var cmpPlayerStatisticsTracker = Engine.QueryInterface(playerEnt, IID_StatisticsTracker);
		return cmpPlayerStatisticsTracker.GetNumUnitsTrained();
	//}
};


/**
 * parameters int player and int feature
 * This will return the requested feature data for the specified player
 * For list of features see FEATURE INFO in ComponentManager.cpp
 */
PlayerManager.prototype.GetPlayerData = function( player, feature )
{
		var playerEnt = this.GetPlayerByID(player);
		var cmpPlayer = Engine.QueryInterface(playerEnt, IID_Player);
		var cmpIdentity = Engine.QueryInterface(playerEnt, IID_Identity);
		var cmpTechnologyManager = Engine.QueryInterface(playerEnt, IID_TechnologyManager)
		var cmpPlayerStatisticsTracker = Engine.QueryInterface(playerEnt, IID_StatisticsTracker);
		var cmpTimer = Engine.QueryInterface(SYSTEM_ENTITY, IID_Timer);
		
		var className = "";
		
		if( feature == -1)
		{
			return cmpTimer.GetTime() / 1000;
		}
		if( feature == 0)
		{
			return cmpTimer.GetTime() / 60000;
		}
		else if( feature == 1 || feature == 6 || feature == 11 || feature == 16 )
		{
			switch ( feature ) 
			{
				case 1:
					return cmpPlayer.getResource(1);
					break;
				case 6:
					return cmpPlayer.getResource(2);
					break;
				case 11:
					return cmpPlayer.getResource(3);
					break;
				case 16:
					return cmpPlayer.getResource(4);
					break;
			}			
		}
		else if( feature == 21 || feature == 25 || feature == 29 || feature == 33 || feature == 37 || feature == 41)
		{
			var classCounts = cmpTechnologyManager.GetClassCounts();
			switch ( feature ) 
			{
				case 21:
					return -1;//classCounts["Cav"];
					break;
				case 25:
					return -1;//classCounts["Cavalry"];
					break;
				case 29:
					return -1;//classCounts["Support"];
					break;
				case 33:
					return -1;//classCounts["Siege"];
					break;
				case 37:
					return -1;//classCounts["Ship"];
					break;
				case 41:
					return -1;//classCounts["Structure"];
					break;
			}
		}
		else
			return cmpPlayerStatisticsTracker.GetPlayerData(feature);
};

//DC
PlayerManager.prototype.GetPlayerStatus = function( player )
{
	var playerEnt = this.GetPlayerByID(player);
	var cmpPlayer = Engine.QueryInterface(playerEnt, IID_Player);
	return cmpPlayer.getStatus();
}

PlayerManager.prototype.GetNumPlayers = function()
{
	return this.playerEntities.length;
};

PlayerManager.prototype.RemoveAllPlayers = function()
{
	// Destroy existing player entities
	for each (var id in this.playerEntities)
	{
		Engine.DestroyEntity(id);
	}
	this.playerEntities = [];
};

PlayerManager.prototype.GetAllPlayerEntities = function()
{
	return this.playerEntities;
};
Engine.RegisterSystemComponentType(IID_PlayerManager, "PlayerManager", PlayerManager);
