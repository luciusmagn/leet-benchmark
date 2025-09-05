require 'benchmark'

def min_steps(s, t)
  min_steps = 0
  h = Hash.new(0)
  s.each_char do |c|
    h[c] += 1
  end

  t.each_char do |c|
    h[c].positive? ? h[c] -= 1 : min_steps += 1
  end
  min_steps
end

# Test cases
test_cases = [
  ["bab", "aba"],
  ["leetcode", "practice"],
  ["anagram", "mangaar"],
  ["a" * 50000, "b" * 50000]  # Large test case
]

# Warmup
100.times do
  test_cases.each { |s, t| min_steps(s, t) }
end

# Internal benchmark
time = Benchmark.realtime do
  10000.times do
    test_cases.each { |s, t| min_steps(s, t) }
  end
end

puts "Ruby internal benchmark: #{(time * 1000).round(2)}ms"
