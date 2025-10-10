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

#pragma once

#define ORYX_CONFIGURATOR

#define SFTLLCK LSFT_T(KC_0)

#define TAPPING_TERM 280
#define QUICK_TAP_TERM 0 // 175
#define COMBO_TERM 40

/* #define DOUBLE_TAP_SHIFT_TURNS_ON_CAPS_WORD 1 */
/* #define CAPS_WORD_INVERT_ON_SHIFT 1 */

// Left-hand home row mods
#define HM_GUI_A LGUI_T(KC_A) // Qwerty and Bépo
#define HM_ALT_S LALT_T(KC_S) // Qwerty
#define HM_ALT_U LALT_T(KC_U) // Bépo
#define HM_SFT_D LSFT_T(KC_D) // Qwerty
#define HM_SFT_I LSFT_T(KC_I) // Bépo
#define HM_CTL_F LCTL_T(KC_F) // Qwerty
#define HM_CTL_E LCTL_T(KC_E) // Bépo
#define HM_HYP_G HYPR_T(KC_G) // Qwerty
#define HM_HYP_COMM HYPR_T(KC_COMM) // Bépo

// Right-hand home row mods
#define HM_HYP_H HYPR_T(KC_H) // Qwerty
#define HM_HYP_C HYPR_T(KC_C) // Bépo
#define HM_CTL_J RCTL_T(KC_J) // Qwerty
#define HM_CTL_T RCTL_T(KC_T) // Bépo
#define HM_SFT_K RSFT_T(KC_K) // Qwerty
#define HM_SFT_S RSFT_T(KC_S) // Bépo
#define HM_ALT_L LALT_T(KC_L) // Qwerty
#define HM_ALT_R LALT_T(KC_R) // Bépo
#define HM_GUI_SCLN RGUI_T(KC_SCLN) // Qwerty
#define HM_GUI_N RGUI_T(KC_N) // Bépo

// French accents
#define FR_QUOT_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_QUOTE) SS_DELAY(50) SS_TAP(X_SPACE)
#define FR_DQUO_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_QUOTE)) SS_DELAY(50) SS_TAP(X_SPACE)
#define FR_GRAVE_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_GRAVE) SS_DELAY(50) SS_TAP(X_SPACE)
#define FR_CIRC_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_6)) SS_DELAY(50) SS_TAP(X_SPACE)
#define FR_E_AIGU_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_QUOTE) SS_DELAY(50) SS_TAP(X_E)           // é
#define FR_E_AIGU_CAPS_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_QUOTE) SS_DELAY(50) SS_RSFT(SS_TAP(X_E))        // É
#define FR_E_GRAVE_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_GRAVE) SS_DELAY(50) SS_TAP(X_E)          // è
#define FR_E_GRAVE_CAPS_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_GRAVE) SS_DELAY(50) SS_RSFT(SS_TAP(X_E)) // È
#define FR_A_GRAVE_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_GRAVE) SS_DELAY(50) SS_TAP(X_A)          // à
#define FR_A_GRAVE_CAPS_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_GRAVE) SS_DELAY(50) SS_RSFT(SS_TAP(X_A))          // À
#define FR_U_GRAVE_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_GRAVE) SS_DELAY(50) SS_TAP(X_U)          // ù
#define FR_C_CEDILLE_M SS_TAP(X_RALT) SS_DELAY(50) SS_TAP(X_COMMA) SS_DELAY(50) SS_TAP(X_C)        // ç
#define FR_A_CIRONFL_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_6)) SS_DELAY(50) SS_TAP(X_A)   // â
#define FR_E_CIRONFL_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_6)) SS_DELAY(50) SS_TAP(X_E)   // ê
#define FR_I_CIRONFL_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_6)) SS_DELAY(50) SS_TAP(X_I)   // î
#define FR_O_CIRONFL_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_6)) SS_DELAY(50) SS_TAP(X_O)   // ô
#define FR_U_CIRONFL_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_6)) SS_DELAY(50) SS_TAP(X_U)   // û
#define FR_E_TREMA_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_QUOTE)) SS_DELAY(50) SS_TAP(X_E) // ë
#define FR_I_TREMA_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_QUOTE)) SS_DELAY(50) SS_TAP(X_I) // ï
#define FR_3_EURO_M SS_TAP(X_RALT) SS_DELAY(50) SS_RSFT(SS_TAP(X_C)) SS_DELAY(50) SS_TAP(X_EQL)    // €

#include "rgb_config.h"

