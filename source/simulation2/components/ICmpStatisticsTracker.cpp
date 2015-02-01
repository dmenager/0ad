/* Copyright (C) 2010 Wildfire Games.
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

#include "ICmpStatisticsTracker.h"

#include "simulation2/system/InterfaceScripted.h"
#include "simulation2/scripting/ScriptComponent.h"

BEGIN_INTERFACE_WRAPPER(StatisticsTracker)
END_INTERFACE_WRAPPER(StatisticsTracker)

class CCmpStatisticsTrackerScripted : public ICmpStatisticsTracker
{
public:
	static void ClassInit(CComponentManager& UNUSED(componentManager)) { } 
	static IComponent* Allocate(ScriptInterface& scriptInterface, jsval instance) 
	{ 
		return new CCmpStatisticsTrackerScripted(scriptInterface, instance); 
	} 
	static void Deallocate(IComponent* cmp) 
	{ 
		delete static_cast<CCmpStatisticsTrackerScripted*> (cmp); 
	} 
	CCmpStatisticsTrackerScripted(ScriptInterface& scriptInterface, jsval instance) : m_Script(scriptInterface, instance) { } 
	static std::string GetSchema() 
	{ 
		return "<a:component type='script-wrapper'/><empty/>"; 
	} 
	virtual void Init(const CParamNode& paramNode) 
	{ 
		m_Script.Init(paramNode, GetEntityId()); 
	} 
	virtual void Deinit() 
	{ 
		m_Script.Deinit(); 
	} 
	virtual void HandleMessage(const CMessage& msg, bool global) 
	{ 
		m_Script.HandleMessage(msg, global); 
	} 
	virtual void Serialize(ISerializer& serialize) 
	{ 
		m_Script.Serialize(serialize); 
	} 
	virtual void Deserialize(const CParamNode& paramNode, IDeserializer& deserialize) 
	{ 
		m_Script.Deserialize(paramNode, deserialize, GetEntityId()); 
	} 
	virtual jsval GetJSInstance() const 
	{ 
		return m_Script.GetInstance(); 
	} 
	virtual int GetComponentTypeId() const 
	{ 
		return 91;//CID_##cname; 
	} 
	//DEFAULT_SCRIPT_WRAPPER(StatisticsTrackerScripted)

	virtual int32_t GetNumWood()
	{
		return m_Script.Call<int32_t>("GetNumUnitsLost");
	}
	private: 
		CComponentTypeScript m_Script; 
};

//expand this to get rid of errors?
//REGISTER_COMPONENT_SCRIPT_WRAPPER(StatisticsTrackerScripted)
