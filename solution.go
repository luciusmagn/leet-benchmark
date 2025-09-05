package main

import (
    "fmt"
    "strings"
    "time"
)

func minSteps(s string, t string) int {
    minSteps := 0
    h := make(map[rune]int)

    for _, c := range s {
        h[c]++
    }

    for _, c := range t {
        if h[c] > 0 {
            h[c]--
        } else {
            minSteps++
        }
    }

    return minSteps
}

func main() {
    testCases := [][2]string{
        {"bab", "aba"},
        {"leetcode", "practice"},
        {"anagram", "mangaar"},
        {strings.Repeat("a", 50000), strings.Repeat("b", 50000)},
    }

    // Warmup
    for i := 0; i < 100; i++ {
        for _, tc := range testCases {
            minSteps(tc[0], tc[1])
        }
    }

    // Internal benchmark
    start := time.Now()
    for i := 0; i < 10000; i++ {
        for _, tc := range testCases {
            minSteps(tc[0], tc[1])
        }
    }
    duration := time.Since(start)

    fmt.Printf("Go internal benchmark: %.2fms\n", duration.Seconds()*1000)
}
