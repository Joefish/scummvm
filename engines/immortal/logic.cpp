/* ScummVM - Graphic Adventure Engine
 *
 * ScummVM is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */

#include "common/system.h"

#include "immortal/console.h"
#include "immortal/immortal.h"
#include "immortal/logic.h"
#include "immortal/graphics.h"
#include "immortal/sound.h"

namespace Immortal {

// TODO:
// Make keys remappable
static const Common::KeyCode keyMap[kKeyNum] = {
	Common::KEYCODE_LEFT,
	Common::KEYCODE_RIGHT,
	Common::KEYCODE_UP,
	Common::KEYCODE_DOWN,
	Common::KEYCODE_RSHIFT,
	Common::KEYCODE_RETURN,
	Common::KEYCODE_ESCAPE
};

Logic::Logic(ImmortalEngine *vm)
	: _console(vm->_console)
	, _resMan(vm->_resMan)
	, _music(vm->_midiPlayer)
	, _screen(vm->_screen)
	, _keyState()
	, _logicState(kLogicStartup)
	, _timeInit(vm->_system->getMillis())
	, _dialog(vm->_screen) {
}

// TODO:
// Parameters for loading savestates/new game/..
// init rooms and gamestate
void Logic::init() {
}

void Logic::update() {
	handleInput();

	switch (_logicState) {
	case kLogicStartup:
		runStartup();
		break;
	case kLogicDialog:
		runDialog();
		break;
	case kLogicGame:
		runGame();
		break;
	case kLogicPause:
		runPause();
		break;
	default:
		error("Game ended up in invalid state!");
		break;
	}
}

void Logic::handleInput() {
	Common::Event event;
	while (g_system->getEventManager()->pollEvent(event)) {
		switch (event.type) {
		case Common::EVENT_KEYDOWN:
			if (event.kbd.keycode == Common::KEYCODE_d &&
				(event.kbd.flags & Common::KBD_CTRL)) {
				_console->attach();
			}
			for (int i = 0; i < kKeyNum; ++i) {
				if (event.kbd.keycode == keyMap[i])
					_keyState[i] = true;
			}
			break;
		case Common::EVENT_KEYUP:
			for (int i = 0; i < kKeyNum; ++i) {
				if (event.kbd.keycode == keyMap[i])
					_keyState[i] = false;
			}
			break;
		default:
			break;
		}
	}
}

void Logic::runStartup() {
	_screen->drawImage(kImageTitleScreen);
	_timer.start();
	if (_timer.elapsedTime() > 3000) {
		_timer.stop();
		loadDialog(kDialogIntro);
		setState(kLogicDialog);
	}
}

void Logic::runDialog() {
	DialogReturnCode rc = _dialog.update();
	if (rc != kDialogRCNotFinished) {

	}
}

void Logic::runGame() {
	_screen->drawImage(kImageScreenFrame);
}

void Logic::runPause() {

}

void Logic::setState(LogicState state) {
	_logicState = state;
}

void Logic::loadDialog(DialogId id) {
	// TODO:
	// Different music for intro, sleep and default
	// Draw health meter as well
	_dialog.load(id);
	_screen->drawImage(kImageScreenFrame);
	if (id == kDialogIntro)
		_music->play(kMusicIntro);
}

}