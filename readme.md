# The AC Compiler

Compiling: `stack build`
Running: `stack exec ac <program>`
Tests: `stack test`

## Example Program
```kotlin
fun character_to_int(c: char): int {
	if (c == '0') { return 0; }
	if (c == '1') { return 1; }
	if (c == '2') { return 2; }
	if (c == '3') { return 3; }
	if (c == '4') { return 4; }
	if (c == '5') { return 5; }
	if (c == '6') { return 6; }
	if (c == '7') { return 7; }
	if (c == '8') { return 8; }
	if (c == '9') { return 9; }
	return 0;
}

fun string_to_int(s: string): int {
	var result: int = 0;
	var i: int = 0;
	while (i < length(s)) {
		var c: char = s[i];
		result = result * 10;
		result = result + character_to_int(c);
		i = i + 1;
	}

	return result;
}

fun fib(n: int): int {
	if ((n == 0) || (n == 1)) {
		return 1;
	}
	return fib(n-1) + fib(n-2);
}

fun main(): unit {
  print("please enter a number: ");
  print(fib(string_to_int(read_line())));
}
```

![running the fibonacci program](/img/fib.png)
