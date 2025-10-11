/* Copyright 2020 ZSA Technology Labs, Inc <@zsa>
 * Copyright 2020 Jack Humbert <jack.humb@gmail.com>
 * Copyright 2020 Christopher Courtney, aka Drashna Jael're  (@drashna) <drashna@live.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include QMK_KEYBOARD_H
#include "version.h"
#include "layers.h"
#include "macros.h"

bool process_record_user(uint16_t keycode, keyrecord_t *record) {
  uint8_t current_layer = get_highest_layer(layer_state);
  uint8_t mod_state = get_mods();
  if (record->event.pressed) {
    switch (keycode) {
      // FR_DQUO on bépo layout
    case VRSN:
      SEND_STRING (QMK_KEYBOARD "/" QMK_KEYMAP " @ " QMK_VERSION);
      return false;
    case SFTLLCK:
      if (record->tap.count) {
	if (record->event.pressed) {
	  // Toggle the lock on the highest layer.
	  layer_lock_invert(get_highest_layer(layer_state));
	}
	return false;
      }
      break;
    case FR_DQUO:
      if (record->event.pressed) {
	// if layer is bepo, and shift is on, send ?
	if (IS_LAYER_ON_STATE(current_layer, BEPO) && (get_mods() & MOD_MASK_SHIFT)) {
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(SS_TAP(X_1));
	  set_mods(mod_state);
	} else {
	  SEND_STRING(FR_DQUO_M);
	}
      }
      break;
    case FR_QUOT:
      // if layer is bepo, and shift is on, send ?
      if (IS_LAYER_ON_STATE(current_layer, BEPO) && (get_mods() & MOD_MASK_SHIFT)) {
	del_mods(MOD_MASK_SHIFT);
	SEND_STRING(SS_LSFT(SS_TAP(X_SLASH)));
	set_mods(mod_state);
      } else {
	SEND_STRING(FR_QUOT_M);
      }
      break;
    case FR_GRAVE:
      if (record->event.pressed) {
	SEND_STRING(FR_GRAVE_M);
      }
      break;
    case FR_CIRC:
      if (record->event.pressed) {
	SEND_STRING(FR_CIRC_M);
      }
      break;
    case FR_E_AIGU:
      if (record->event.pressed) {
	if (get_mods() & MOD_MASK_SHIFT) {
	  // É
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(FR_E_AIGU_CAPS_M);
	  set_mods(mod_state);
	} else {
	  // é
	  SEND_STRING(FR_E_AIGU_M);
	}
      }
      break;
    case FR_E_AIGU_CAPS:
      if (record->event.pressed) {
	SEND_STRING(FR_E_AIGU_CAPS_M);
      }
      break;
    case FR_E_GRAVE:
      if (record->event.pressed) {
	if (get_mods() & MOD_MASK_SHIFT) {
	  // 
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(FR_E_GRAVE_CAPS_M);
	  set_mods(mod_state);
	} else {
	  // è
	  SEND_STRING(FR_E_GRAVE_M); 
	}
      }
      break;
    case FR_E_GRAVE_CAPS:
      if (record->event.pressed) {
	SEND_STRING(FR_E_GRAVE_CAPS_M);
      }
      break;
    case FR_A_GRAVE:
      if (record->event.pressed) {
	if (get_mods() & MOD_MASK_SHIFT) {
	  del_mods(MOD_MASK_SHIFT);
	  SEND_STRING(FR_A_GRAVE_CAPS_M);
	  set_mods(mod_state);
	} else {
	  // à
	  SEND_STRING(FR_A_GRAVE_M); 
	}
      }
      break;
    case FR_A_GRAVE_CAPS:
      if (record->event.pressed) {
	SEND_STRING(FR_A_GRAVE_CAPS_M);
      }
      break;
    }
  }
  return true;
}
