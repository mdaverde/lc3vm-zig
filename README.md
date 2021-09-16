# LC3vm-zig

Written entirely in Zig, this project implements a virtual machine for LC-3 programs, an educational ISA.

## About LC-3

 > Little Computer 3, or LC-3, is a type of computer educational programming language, an assembly language, which is a type of low-level programming language. The LC-3 was developed by Yale N. Patt at the University of Texas at Austin and Sanjay J. Patel at the University of Illinois at Urbana–Champaign. Their specification of the instruction set, the overall architecture of the LC-3, and a hardware implementation can be found in their textbook "Introduction to Computing Systems."

[Wikipedia](https://en.wikipedia.org/wiki/LC-3#cite_note-CompSysBook-1)

## Instruction Set Architecture (ISA)

The specification for the ISA can be found [here](https://justinmeiners.github.io/lc3-vm/supplies/lc3-isa.pdf)

## Usage

### Dependencies

Development was done using the, at the time, latest master branch of [Zig](https://github.com/ziglang/zig) on a Linux x86_64 machine.

```shell
$ zig version
0.9.0-dev.952+0c091feb5
```

### Build

```shell
$ zig build
```

### Testing

```shell
$ zig test ./test.zig
All tests passed.
```

## References

For original inspiration, see [lc3-vm](https://github.com/justinmeiners/lc3-vm)

## License

MIT - Maintained by [Milan](https://mdaverde.com)
