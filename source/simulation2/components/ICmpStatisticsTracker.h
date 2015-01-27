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

#ifndef INCLUDED_ICMPSTATISTICSTRACKER
#define INCLUDED_ICMPSTATISTICSTRACKER

#include "simulation2/system/Interface.h"

/**
 * Player manager. This maintains the list of players that exist in the game.
 */
class ICmpStatisticsTracker : public IComponent
{
public:
	virtual int32_t GetNumWood();

	virtual bool NewJSObject(ScriptInterface& scriptInterface, JS::MutableHandleObject out) const;
	static void InterfaceInit(ScriptInterface& scriptInterface);
	static int GetInterfaceId() { return 91; }
	//DECLARE_INTERFACE_TYPE(StatisticsTracker)
};

#endif // INCLUDED_ICMPSTATISTICSTRACKER
