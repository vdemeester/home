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

// Key override declarations
extern const key_override_t circ_exclamation_override;
extern const key_override_t dot_colon_override;
extern const key_override_t comma_semicolon_override;
extern const key_override_t ldaq_two_override;
extern const key_override_t rdaq_three_override;
extern const key_override_t lprn_four_override;
extern const key_override_t rprn_five_override;
extern const key_override_t at_six_override;
extern const key_override_t plus_seven_override;
extern const key_override_t minus_eight_override;
extern const key_override_t slash_nine_override;
extern const key_override_t star_zero_override;
extern const key_override_t b_pipe_override;
extern const key_override_t b_brkp_override;

// Export the key_overrides array for QMK introspection
extern const key_override_t *key_overrides[];
