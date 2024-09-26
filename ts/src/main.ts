import fs from 'fs';

const MEMORY_SIZE = 32768;

class InputHandler {
    static INSTANCE = new InputHandler();
    private buffer: string[] = [];
    private stdinClosed = false;
    private stopWaiting: (() => void) | null = null;
    private constructor() {
        process.stdin.on('data', (data) => {
            const input = data.toString();
            for (const c of input) {
                if (c.charCodeAt(0) == 26) {
                    this.stdinClosed = true;
                }
                if (!this.stdinClosed) {
                    this.buffer.push(c);
                }
            }
            if (this.stopWaiting) {
                this.stopWaiting();
            }
        });
    }

    async getInput() {
        const self = this;
        while (this.buffer.length == 0) {
            if (this.stdinClosed) {
                console.log('Trying to read input after stdin is closed');
                throw new Error();
            }
            await new Promise<void>((resolve) => {
                if (self.stopWaiting) {
                    self.stopWaiting();
                }
                self.stopWaiting = resolve;
            });
        }

        return this.buffer.shift()?.charCodeAt(0);
    }
}

const main = async () => {
    const memory = new Array(MEMORY_SIZE).fill(0);
    const stack: number[] = [];
    let pointer = 0;

    if (process.argv.length < 3) {
        console.log('usage: bfi <file name>');
        return 1;
    }

    const fileName = process.argv[2];
    const code = fs.readFileSync(fileName).toString();

    for (let pc = 0; pc < code.length; pc++) {
        const command = code.charAt(pc);
        if (command == '+') {
            memory[pointer]++;
            if (memory[pointer] == 256) {
                memory[pointer] = 0;
            }
        } else if (command == '-') {
            memory[pointer]--;
            if (memory[pointer] == -1) {
                memory[pointer] = 255;
            }
        } else if (command == '>') {
            pointer++;
            if (pointer >= MEMORY_SIZE) {
                console.log(`Pointer out of upper bound ${MEMORY_SIZE}!`);
                return 1;
            }
        } else if (command == '<') {
            pointer--;
            if (pointer < 0) {
                console.log('Pointer out of lower bound 0!');
                return 1;
            }
        } else if (command == ',') {
            try {
                memory[pointer] = await InputHandler.INSTANCE.getInput();
            } catch (e) {
                return 1;
            }
        } else if (command == '.') {
            process.stdout.write(String.fromCharCode(memory[pointer]));
        } else if (command == '[') {
            if (memory[pointer] == 0) {
                while (true) {
                    pc++;
                    if (code[pc] == '[') {
                        stack.push(-1);
                    } else if (code[pc] == ']') {
                        if (stack.length == 0 || stack[stack.length - 1] != -1) {
                            break;
                        }
                        stack.pop();
                    }
                }
            } else {
                stack.push(pc);
            }
        } else if (command == ']') {
            if (memory[pointer] != 0) {
                if (stack.length == 0) {
                    console.log(`Missing '[' of ']' at position ${pc + 1}`);
                    return 1;
                }
                pc = stack[stack.length - 1];
            } else if (stack.length != 0) {
                stack.pop();
            }
        }
    }
};

const returnValue = await main();
process.stdout.end();
process.exit(returnValue);
