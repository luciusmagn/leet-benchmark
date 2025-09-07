import time

class Solution:
    def minSteps(self, s: str, t: str) -> int:
        d = {}
        count = 0
        d = {x: s.count(x) for x in set(s)}
        # for c in s:
        #     d[c] = 1 if not d.get(c) else d[c] + 1


        count = sum([max(0, t.count(x)-d.get(x, 0)) for x in set(t)])
        # for c in t:
        #     if d.get(c):
        #         d[c] -= 1
        #     else:
        #         count += 1

        return count


# Test cases
test_cases = [
    ("bab", "aba"),
    ("leetcode", "practice"),
    ("anagram", "mangaar"),
    ("a" * 50000, "b" * 50000)  # Large test case
]

solution = Solution()

# Warmup
for _ in range(100):
    for s, t in test_cases:
        solution.minSteps(s, t)

# Internal benchmark
start = time.perf_counter()
for _ in range(10000):
    for s, t in test_cases:
        solution.minSteps(s, t)
end = time.perf_counter()

print(f"Python internal benchmark: {(end - start) * 1000:.2f}ms")
