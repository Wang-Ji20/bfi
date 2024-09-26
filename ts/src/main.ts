import fs from 'fs';

const MEMORY_SIZE = 32768;

class InputHandler {
    static INSTANCE = new InputHandler();
    private buffer: string[] = [];
    private stopWaiting: (() => void) | null = null;
    private constructor() {
        process.stdin.on('data', (data) => {
            const input = data.toString();
            for (const c of input) {
                this.buffer.push(c);
            }
            if (this.stopWaiting) {
                this.stopWaiting();
            }
        });
    }

    async getInput() {
        const self = this;
        while (this.buffer.length == 0) {
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
            if (memory[pointer] == 128) {
                memory[pointer] = -128;
            }
        } else if (command == '-') {
            memory[pointer]--;
            if (memory[pointer] == -129) {
                memory[pointer] = 127;
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
            memory[pointer] = await InputHandler.INSTANCE.getInput();
        } else if (command == '.') {
            process.stdout.write(String.fromCharCode(memory[pointer]));
        } else if (command == '[') {
            stack.push(pc);
            if (memory[pointer] == 0) {
                while (code[pc] != ']') {
                    pc++;
                    if (pc >= code.length) {
                        console.log(`Missing ']' of '[' at position ${pc + 1}`);
                        return 1;
                    }
                }
            }
        } else if (command == ']') {
            if (stack.length == 0) {
                console.log(`Missing '[' of ']' at position ${pc + 1}`);
                return 1;
            }
            if (memory[pointer] != 0) {
                pc = stack[stack.length - 1];
            } else {
                stack.pop();
            }
        }
    }
};

const returnValue = await main();
process.stdout.end();
process.exit(returnValue);
