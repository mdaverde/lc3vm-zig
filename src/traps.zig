const Traps = enum(u16) {
    GETC,   // Read a single character
    OUT,    // Write character to console
    PUTS,   // Write string to console
    IN,     // Print prompt and read
    PUTSP,  //
    HALT,   // Halt execution and print message to console
}