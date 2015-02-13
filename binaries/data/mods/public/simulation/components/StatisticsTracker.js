function StatisticsTracker() {}

StatisticsTracker.prototype.Schema =
	"<a:component type='system'/><empty/>";

StatisticsTracker.prototype.Init = function()
{
	// units
	this.unitsClasses = [
		"Infantry",
		"Worker",
		"Female",
		"Cavalry",
		"Champion",
		"Hero",
		"Ship"
	];	
	this.unitsTrained = {
		"Infantry": 0,
		"Worker": 0,
		"Female": 0,
		"Cavalry": 0,
		"Champion": 0,
		"Hero": 0,
		"Ship": 0,
		"total": 0
	};
	this.unitsLost = {
		"Infantry": 0,
		"Worker": 0,
		"Female": 0,
		"Cavalry": 0,
		"Champion": 0,
		"Hero": 0,
		"Ship": 0,
		"total": 0
	};
	this.unitsLostValue = 0;
	this.enemyUnitsKilled = {
		"Infantry": 0,
		"Worker": 0,
		"Female": 0,
		"Cavalry": 0,
		"Champion": 0,
		"Hero": 0,
		"Ship": 0,
		"total": 0
	};
	this.enemyUnitsKilledValue = 0;
	// buildings
	this.buildingsClasses = [
		"House",
		"Economic",
		"Outpost",
		"Military",
		"Fortress",
		"CivCentre",
		"Wonder"
	];
	this.buildingsConstructed = {
		"House": 0,
		"Economic": 0,
		"Outpost": 0,
		"Military": 0,
		"Fortress": 0,
		"CivCentre": 0,
		"Wonder": 0,
		"total": 0	
	};
	this.buildingsLost = {
		"House": 0,
		"Economic": 0,
		"Outpost": 0,
		"Military": 0,
		"Fortress": 0,
		"CivCentre": 0,
		"Wonder": 0,
		"total": 0
		};
	this.buildingsLostValue = 0;
	this.enemyBuildingsDestroyed = {
		"House": 0,
		"Economic": 0,
		"Outpost": 0,
		"Military": 0,
		"Fortress": 0,
		"CivCentre": 0,
		"Wonder": 0,
		"total": 0
		};
	this.enemyBuildingsDestroyedValue = 0;
	// resources
	this.resourcesGathered = {
		"food": 0,
		"wood": 0,
		"metal": 0,
		"stone": 0,
		"vegetarianFood": 0
	};
	this.resourcesUsed = {
		"food": 0,
		"wood": 0,
		"metal": 0,
		"stone": 0
	};
	this.resourcesSold = {
		"food": 0,
		"wood": 0,
		"metal": 0,
		"stone": 0
	};
	this.resourcesBought = {
		"food": 0,
		"wood": 0,
		"metal": 0,
		"stone": 0
	};
	this.tributesSent = 0;
	this.tributesReceived = 0;
	this.tradeIncome = 0;
	this.treasuresCollected = 0;
};

StatisticsTracker.prototype.GetNumUnitsTrained = function()
{
	
	return this.unitsTrained.total;
};

StatisticsTracker.prototype.GetPlayerData = function( feature )
{

	switch ( feature ) {
	case 1:
		return (this.resourcesGathered.food + this.resourcesBought.food - this.resourcesUsed.food - this.resourcesSold.food + 300);
		break;
	case 2:
		return this.resourcesGathered.wood + this.resourcesBought.wood - this.resourcesUsed.wood - this.resourcesSold.wood + 300;
		break;
	case 3:
		return this.resourcesGathered.stone + this.resourcesBought.stone - this.resourcesUsed.stone - this.resourcesSold.stone + 300;
		break;
	case 4:
		return this.resourcesGathered.metal + this.resourcesBought.metal - this.resourcesUsed.metal - this.resourcesSold.metal + 300;
		break;
	case 5:
		return this.unitsTrained.Infantry - this.unitsLost.Infantry + 4;
		break;
	case 6:
		return this.unitsTrained.Worker - this.unitsLost.Worker + 8;
		break;
	case 7:
		return this.unitsTrained.Female - this.unitsLost.Female + 4;
		break;
	case 8:
		return this.unitsTrained.Cavalry - this.unitsLost.Cavalry + 1;
		break;
	case 9:
		return this.unitsTrained.Champion - this.unitsLost.Champion;
		break;
	case 10:
		return this.unitsTrained.Hero - this.unitsLost.Hero;
		break;
	case 11:
		return this.unitsTrained.Ship - this.unitsLost.Ship;
		break;
	case 12:
		return this.buildingsConstructed.House - this.buildingsLost.House;
		break;
	case 13:
		return this.buildingsConstructed.Economic - this.buildingsLost.Economic;
		break;
	case 14:
		return this.buildingsConstructed.Outpost - this.buildingsLost.Outpost;
		break;
	case 15:
		return this.buildingsConstructed.Military - this.buildingsLost.Military;
		break;
	case 16:
		return this.buildingsConstructed.Fortress - this.buildingsLost.Fortress;
		break;
	case 17:
		return this.buildingsConstructed.CivCentre - this.buildingsLost.CivCentre + 1;
		break;
	case 18:
		return this.buildingsConstructed.Wonder - this.buildingsLost.Wonder;
		break;
	case 19:
		return this.enemyUnitsKilled.total;
		break;
	case 20:
		return this.enemyBuildingsDestroyed.total;
		break;
	case 21:
		return this.unitsLost.total;
		break;
	case 22:
		return this.buildingsLost.total;
		break;
}	
	//return this.unitsTrained.total;
};

StatisticsTracker.prototype.GetStatistics = function()
{
	return {
		"unitsTrained": this.unitsTrained,
		"unitsLost": this.unitsLost,
		"unitsLostValue": this.unitsLostValue,
		"enemyUnitsKilled": this.enemyUnitsKilled,
		"enemyUnitsKilledValue": this.enemyUnitsKilledValue,
		"buildingsConstructed": this.buildingsConstructed,
		"buildingsLost": this.buildingsLost,
		"buildingsLostValue": this.buildingsLostValue,
		"enemyBuildingsDestroyed": this.enemyBuildingsDestroyed,
		"enemyBuildingsDestroyedValue": this.enemyBuildingsDestroyedValue,
		"resourcesGathered": this.resourcesGathered,
		"resourcesUsed": this.resourcesUsed,
		"resourcesSold": this.resourcesSold,
		"resourcesBought": this.resourcesBought,
		"tributesSent": this.tributesSent,
		"tributesReceived": this.tributesReceived,
		"tradeIncome": this.tradeIncome,
		"treasuresCollected": this.treasuresCollected,
		"percentMapExplored": this.GetPercentMapExplored()
	};
};

/**
 * Increments counter associated with certain entity/counter and type of given entity.
 * @param cmpIdentity The entity identity component
 * @param counter The name of the counter to increment (e.g. "unitsTrained")
 * @param type The type of the counter (e.g. "workers")
 */
StatisticsTracker.prototype.CounterIncrement = function(cmpIdentity, counter, type)
{
	var classes = cmpIdentity.GetClassesList();
	if (!classes)
		return;
	if (classes.indexOf(type) != -1)
		this[counter][type]++;
};

/** 
 * Counts the total number of units trained as well as an individual count for 
 * each unit type. Based on templates.
 * @param trainedUnit The unit that has been trained 
 */ 
StatisticsTracker.prototype.IncreaseTrainedUnitsCounter = function(trainedUnit)
{
	var cmpUnitEntityIdentity = Engine.QueryInterface(trainedUnit, IID_Identity);

	if (!cmpUnitEntityIdentity)
		return;

	for each (var type in this.unitsClasses)
		this.CounterIncrement(cmpUnitEntityIdentity, "unitsTrained", type);

	this.unitsTrained.total++;
};

/** 
 * Counts the total number of buildings constructed as well as an individual count for 
 * each building type. Based on templates.
 * @param constructedBuilding The building that has been constructed 
 */ 
StatisticsTracker.prototype.IncreaseConstructedBuildingsCounter = function(constructedBuilding)
{
	var cmpBuildingEntityIdentity = Engine.QueryInterface(constructedBuilding, IID_Identity);
		
	if (!cmpBuildingEntityIdentity)
		return;

	for each(var type in this.buildingsClasses)
		this.CounterIncrement(cmpBuildingEntityIdentity, "buildingsConstructed", type);

	this.buildingsConstructed.total++;
};

StatisticsTracker.prototype.KilledEntity = function(targetEntity)
{
	var cmpTargetEntityIdentity = Engine.QueryInterface(targetEntity, IID_Identity);
	var cmpCost = Engine.QueryInterface(targetEntity, IID_Cost);
	var costs = cmpCost.GetResourceCosts();
	if (!cmpTargetEntityIdentity)
		return;

	var cmpFoundation = Engine.QueryInterface(targetEntity, IID_Foundation);
	// We want to deal only with real structures, not foundations
	var targetIsStructure = cmpTargetEntityIdentity.HasClass("Structure") && cmpFoundation == null;
	var targetIsDomesticAnimal = cmpTargetEntityIdentity.HasClass("Animal") && cmpTargetEntityIdentity.HasClass("Domestic");
	// Don't count domestic animals as units
	var targetIsUnit = cmpTargetEntityIdentity.HasClass("Unit") && !targetIsDomesticAnimal;

	var cmpTargetOwnership = Engine.QueryInterface(targetEntity, IID_Ownership);
    
	// Don't increase counters if target player is gaia (player 0)
	if (cmpTargetOwnership.GetOwner() == 0)
		return;

	if (targetIsUnit)
	{
		for each (var type in this.unitsClasses)
			this.CounterIncrement(cmpTargetEntityIdentity, "enemyUnitsKilled", type);

		this.enemyUnitsKilled.total++;
		
		for each (var cost in costs)
			this.enemyUnitsKilledValue += cost;
	}	
	if (targetIsStructure)
	{
		for each (var type in this.buildingsClasses)
			this.CounterIncrement(cmpTargetEntityIdentity, "enemyBuildingsDestroyed", type);

		this.enemyBuildingsDestroyed.total++;
		
		for each (var cost in costs)
			this.enemyBuildingsDestroyedValue += cost;
	}
};

StatisticsTracker.prototype.LostEntity = function(lostEntity)
{
	var cmpLostEntityIdentity = Engine.QueryInterface(lostEntity, IID_Identity);
	var cmpCost = Engine.QueryInterface(lostEntity, IID_Cost);
	var costs = cmpCost.GetResourceCosts();
	if (!cmpLostEntityIdentity)
		return;
	
	var cmpFoundation = Engine.QueryInterface(lostEntity, IID_Foundation);
	// We want to deal only with real structures, not foundations
	var lostEntityIsStructure = cmpLostEntityIdentity.HasClass("Structure") && cmpFoundation == null;
	var lostEntityIsDomesticAnimal = cmpLostEntityIdentity.HasClass("Animal") && cmpLostEntityIdentity.HasClass("Domestic");
	// Don't count domestic animals as units
	var lostEntityIsUnit = cmpLostEntityIdentity.HasClass("Unit") && !lostEntityIsDomesticAnimal;

	if (lostEntityIsUnit)
	{
		for each (var type in this.unitsClasses)
			this.CounterIncrement(cmpLostEntityIdentity, "unitsLost", type);

		this.unitsLost.total++;
		
		for each (var cost in costs)
			this.unitsLostValue += cost;	
	}	
	if (lostEntityIsStructure)
	{
		for each (var type in this.buildingsClasses)
			this.CounterIncrement(cmpLostEntityIdentity, "buildingsLost", type);

		this.buildingsLost.total++;
		
		for each (var cost in costs)
			this.buildingsLostValue += cost;
	}
};

/**
 * @param type Generic type of resource (string)
 * @param amount Amount of resource, whick should be added (integer)
 * @param specificType Specific type of resource (string, optional)
 */
StatisticsTracker.prototype.IncreaseResourceGatheredCounter = function(type, amount, specificType)
{
	this.resourcesGathered[type] += amount;
	
	if (type == "food" && (specificType == "fruit" || specificType == "grain"))
		this.resourcesGathered.vegetarianFood += amount;
};

/**
 * @param type Generic type of resource (string)
 * @param amount Amount of resource, whick should be added (integer)
 */
StatisticsTracker.prototype.IncreaseResourceUsedCounter = function(type, amount)
{
	this.resourcesUsed[type] += amount;
};

StatisticsTracker.prototype.IncreaseTreasuresCollectedCounter = function()
{
	this.treasuresCollected++;
};

StatisticsTracker.prototype.IncreaseResourcesSoldCounter = function(type, amount)
{
	this.resourcesSold[type] += amount;
};

StatisticsTracker.prototype.IncreaseResourcesBoughtCounter = function(type, amount)
{
	this.resourcesBought[type] += amount;
};

StatisticsTracker.prototype.IncreaseTributesSentCounter = function(amount)
{
	this.tributesSent += amount;
};

StatisticsTracker.prototype.IncreaseTributesReceivedCounter = function(amount)
{
	this.tributesReceived += amount;
};

StatisticsTracker.prototype.IncreaseTradeIncomeCounter = function(amount)
{
	this.tradeIncome += amount;
};

StatisticsTracker.prototype.GetPercentMapExplored = function()
{
	var cmpRangeManager = Engine.QueryInterface(SYSTEM_ENTITY, IID_RangeManager);
	var cmpPlayer = Engine.QueryInterface(this.entity, IID_Player);
	return cmpRangeManager.GetPercentMapExplored(cmpPlayer.GetPlayerID());
};

Engine.RegisterComponentType(IID_StatisticsTracker, "StatisticsTracker", StatisticsTracker);
