function PlayerManager() {}

PlayerManager.prototype.Schema =
	"<a:component type='system'/><empty/>";

PlayerManager.prototype.Init = function()
{
	this.playerEntities = []; // list of player entity IDs
	
	// KW: Keep last average position of units for each player (6 players possible)
	this.lastAvgUnitPosZ = { "military" : [0,0,0,0,0,0],
							  "support"  : [0,0,0,0,0,0] };
	this.lastAvgUnitPosX = { "military" : [0,0,0,0,0,0],
							  "support"  : [0,0,0,0,0,0] };
	
	// KW: The amount of distance for it to be considered a movement of units
	this.posThreshold = 50;
};

PlayerManager.prototype.SetLastX = function(x, player, feature)
{
	if (feature == 45)
	{
		this.lastAvgUnitPosX["military"][player-1] = x;
	}
	else if (feature == 46) 
	{
		this.lastAvgUnitPosX["support"][player-1] = x;
	}
};

PlayerManager.prototype.SetLastZ = function(z, player, feature)
{
	if (feature == 45)
	{
		this.lastAvgUnitPosZ["military"][player-1] = z;
	}
	else if (feature == 46) 
	{
		this.lastAvgUnitPosZ["support"][player-1] = z;
	}
};

PlayerManager.prototype.GetLastX = function( player, feature )
{
	if (feature == 45)
	{
		return this.lastAvgUnitPosX["military"][player-1];
	}
	else if (feature == 46)
	{
		return this.lastAvgUnitPosX["support"][player-1];
	}
	else 
	{
		return 0;
	}
};

PlayerManager.prototype.GetLastZ = function( player, feature )
{
	if (feature == 45)
	{
		return this.lastAvgUnitPosZ["military"][player-1];
	}
	else if (feature == 46)
	{
		return this.lastAvgUnitPosZ["support"][player-1];
	}
	else
	{
		return 0;
	}
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
		// KW: TODO: feature 46 - support movement. 
		// Still need to change game.cpp and componentManager.cpp
		// Also need to get the first position ever - find a function that determines the first position of every unit
		else if (feature == 45 || feature == 46) {
			return this.GetAverageUnitsMovement(player, feature);
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

// KW : function to get bool of average movements
//    1 = movements occurred past posThreshold
//    0 = movements did not occur past posThreshold 
PlayerManager.prototype.GetAverageUnitsMovement = function( player, feature )
{
		// Open GuiInterface Interface
	var cmpGuiInterface = Engine.QueryInterface(SYSTEM_ENTITY, IID_GuiInterface);
	
	var count = 0;		
	var positionTotalX = 0;
	var positionTotalZ = 0;
	
	// Get all the entities that belong to the player
	var pEntities = cmpGuiInterface.GetPlayerEntities(player);
	
	// Iterate through each entity
	for (var i = 0; i < pEntities.length; i++) {
		
		// Get information on the entities
		var entState = cmpGuiInterface.GetEntityState (player, pEntities[i]);
		var entExtendedState = cmpGuiInterface.GetExtendedEntityState (player, pEntities[i]);
		if (entState.identity) {	
			for (var j = 0; j < entState.identity.classes.length; j++) {
				// Feature for military units (can attack)
				if (feature == 45 && 
					(entState.identity.classes[j] == "Soldier" 
					|| entState.identity.classes[j] == "Siege")) { 
					count++;
					
					positionTotalX += entState.position.x;
					positionTotalZ += entState.position.z;
					break;
				}
				// Feature for support units
				else if (feature == 46 && entState.identity.classes[j] == "Support") {
					count++;
					
					positionTotalX += entState.position.x;
					positionTotalZ += entState.position.z;
					break;
				}
			}
		}
	}
	
	// Get average of X and Z axis
	var avgTroopPosX = positionTotalX/count;
	var avgTroopPosZ = positionTotalZ/count;
	
	// Get the distance between (current average x and previous average x) and (current average z and previous average z)
	var avgTroopMoveX = avgTroopPosX - this.GetLastX(player, feature);
	var avgTroopMoveZ = avgTroopPosZ - this.GetLastZ(player, feature);
	
	// If the distance between the previous average point and current average point is different by a threshold
	if (Math.sqrt((avgTroopMoveX * avgTroopMoveX) +
				  (avgTroopMoveZ * avgTroopMoveZ))
		> this.posThreshold)
	{
		// Set previous point to current point
		this.SetLastX(avgTroopPosX, player, feature);
		this.SetLastZ(avgTroopPosZ, player, feature);
		return 1; // Movements occurred
	}
	else
	{
		// Set previous point to current point
		this.SetLastX(avgTroopPosX, player, feature);
		this.SetLastZ(avgTroopPosZ, player, feature);
		return 0; // Movements did not occur :(
	}
};

Engine.RegisterSystemComponentType(IID_PlayerManager, "PlayerManager", PlayerManager);
