# Moonlander Keyboard Configuration

This directory contains keyboard configuration files for the ZSA Moonlander keyboard.

## Files

- `config/keymap.c` - QMK firmware keymap configuration
- `config/config.h` - QMK firmware configuration header
- `kanata.kbd` - Kanata configuration (software keyboard remapper)

## Kanata Configuration

The `kanata.kbd` file is a Kanata configuration that replicates the QMK keymap functionality. Kanata is a cross-platform software keyboard remapper that can be used as an alternative to or in addition to QMK firmware.

### Layers

The configuration includes the following layers:

1. **qwer** (default) - QWERTY layout with home row mods
2. **bepo** - French Bépo layout with home row mods
3. **ergl** - Ergol layout with home row mods
4. **symb** - Symbols layer
5. **numb** - Numbers and function keys layer
6. **navi** - Navigation and media keys layer
7. **mous** - Mouse control layer

### Home Row Mods

The configuration implements home row modifications (tap for key, hold for modifier) matching the QMK configuration:

**QWERTY/Ergol Layout:**
- Left hand: A (Cmd/Meta), S (Alt), D (Shift), F (Ctrl), G (Hyper)
- Right hand: H (Hyper), J (Ctrl), K (Shift), L (Alt), ; (Cmd/Meta)

**Bépo Layout:**
- Left hand: A (Cmd/Meta), U (Alt), I (Shift), E (Ctrl), , (Hyper)
- Right hand: C (Hyper), T (Ctrl), S (Shift), R (Alt), N (Cmd/Meta)

### Layer Access

- **Space** (tap) / **NUMB layer** (hold)
- **Backspace** (tap) / **NAVI layer** (hold)
- **Enter** (tap) / **SYMB layer** (hold)

### French Accents (Bépo Layer)

The Bépo layer includes support for French accented characters using compose key sequences (AltGr/Right Alt):
- `@fr_dquo` - Double quote with space
- `@fr_quot` - Single quote with space
- `@fr_grave` - Grave accent with space
- `@fr_circ` - Circumflex with space
- `@fr_e_aigu` - é (e with acute accent)
- `@fr_e_grave` - è (e with grave accent)
- `@fr_a_grave` - à (a with grave accent)

### Usage

To use this configuration with Kanata:

1. Install Kanata from https://github.com/jtroo/kanata
2. Run Kanata with the configuration file:
   ```bash
   kanata -c kanata.kbd
   ```

### Differences from QMK

Some QMK features are not directly available in Kanata:

- **QK_REP** (Repeat key) - Mapped to no-op (XX)
- **QK_AREP** (Alternate repeat) - Mapped to no-op (XX)
- **VRSN** (Version macro) - Mapped to no-op (XX)
- **RGB/LED controls** - Not applicable in software remapping
- **Combos** - Not implemented in this version (can be added)
- **Mouse keys** - Require additional OS-level configuration

### Customization

You can customize the configuration by:

1. Adjusting timing values in the `defvar` section
2. Modifying layer definitions in the `deflayer` sections
3. Adding or removing aliases in the `defalias` section
4. Changing the key mappings to match your preferences

### Note on Layout Approximation

The Moonlander has a unique 72-key layout that doesn't directly map to a standard keyboard. The `defsrc` section uses a standard keyboard layout as an approximation. When using Kanata with the actual Moonlander hardware, you may need to adjust the key positions based on your specific use case.

## QMK Configuration

The QMK configuration in the `config/` directory is the firmware that runs on the Moonlander keyboard itself. To build and flash the QMK firmware:

```bash
# Navigate to your QMK firmware directory
cd ~/qmk_firmware

# Build the firmware
qmk compile -kb moonlander -km vdemeester

# Flash to keyboard
qmk flash -kb moonlander -km vdemeester
```

See the QMK documentation for more details: https://docs.qmk.fm/

## Comparison: QMK vs Kanata

| Feature | QMK (Firmware) | Kanata (Software) |
|---------|----------------|-------------------|
| **Where it runs** | On keyboard hardware | On your computer (OS level) |
| **Compatibility** | Specific to QMK-compatible keyboards | Works with any keyboard |
| **Latency** | Very low (hardware) | Low (software, depends on OS) |
| **Configuration** | C code, compile & flash | Text config file, reload on change |
| **Portability** | Tied to keyboard | Follows you across keyboards |
| **RGB/LED control** | ✅ Full control | ❌ Not applicable |
| **Combos** | ✅ Supported | ✅ Supported (not in this config yet) |
| **Tap-hold/Mod-tap** | ✅ Supported | ✅ Supported |
| **Layer switching** | ✅ Supported | ✅ Supported |
| **Macros** | ✅ Supported | ✅ Supported |
| **Mouse keys** | ✅ Built-in | ⚠️ Requires OS configuration |

## Related

- Kanata: https://github.com/jtroo/kanata
- QMK Firmware: https://qmk.fm/
- ZSA Moonlander: https://www.zsa.io/moonlander/
