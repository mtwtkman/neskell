# References
- [dev article](https://yizhang82.dev/nes-emu-overview)
- [guide](https://www.nesdev.org/wiki/NES_reference_guide)

# Hardware specs
## CPU
Works for:

Necessary emulating `8-bit 6502 CPU`.

It has some types registers

| name | use for |
| --- | --- |
| A/X/Y | general|
| P | status |
| SP | stack pointer |
| PC | program counter or instruction pointer |

## Memory
Necessary 16-bit addressable memory space.
Actually RAM is limitted in 2KB.

## PPU
Rendering system.

It has 256x240 screen which is composed by 8x8 tiles for background.

## APU
Supporting channels are:

- 2 pulse channel
- 1 triangle channel
- 1 noise channel
- 1 delta modulation channel

## Controller
NES controller and NES mouse(!).

## Cartridge
This is exactly game it self and extends resources.

That owns:
- game data

That maybe owns:
- battery-backed RAM
- audio process unit

# Developing Roadmap
[This](https://yizhang82.dev/nes-emu-overview#have-a-plan) guiding well.

# Test roms
[here](https://github.com/christopherpow/nes-test-roms)
