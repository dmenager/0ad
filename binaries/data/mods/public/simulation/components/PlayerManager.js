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
 */
PlayerManager.prototype.GetPlayerData = function( player, feature )
{
		var playerEnt = this.GetPlayerByID(player);
		var cmpPlayer = Engine.QueryInterface(playerEnt, IID_Player);
		var cmpPlayerStatisticsTracker = Engine.QueryInterface(playerEnt, IID_StatisticsTracker);
		
		if( feature < 5 )
		{
			return cmpPlayer.getResource(feature);			
		}
		else
			return cmpPlayerStatisticsTracker.GetPlayerData(feature);
};

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
