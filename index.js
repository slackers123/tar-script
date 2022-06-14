function fib(a, b, d) {
    if (d < 40) {
        console.log(a);
        fib(b, a+b, d+1);
    }
}

for (let i = 0; i < 100; i++) {
    fib(1, 1, 0);
}