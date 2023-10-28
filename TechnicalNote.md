# References
- [dev article](https://yizhang82.dev/nes-emu-overview)
- [guide](https://www.nesdev.org/wiki/NES_reference_guide)
- [6502 CPU spec](http://6502.org/users/obelisk/6502/)

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

# Modeling
## Processing flow
I can think about flow of playing with NES:

- Player inserts a cartridge
- Then the cartidge maps its ROM data into NES's memory
- Then CPU reads ROM data which was mapped on memory
- Then CPU waits PPU initialization
- Then CPU writes read presentation data into PPU
- Also CPU writes read sound data into APU
- Then CPU waits user input via controller (Event loop is running)

## Event handling
CPU looks just an event handler and other hardwares are individual from CPU (Maybe we can call them event bus).
So they run parallel each other.

NES emulator needs functionality which works for making all hardware can communicate to each other. That is called "Master Clock" generally.


# Developing Roadmap
[This](https://yizhang82.dev/nes-emu-overview#have-a-plan) guiding well.

# Test roms
[here](https://github.com/christopherpow/nes-test-roms)
