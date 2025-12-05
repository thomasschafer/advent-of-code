#!/usr/bin/env bash
# Usage: ./add-day.sh 02
set -euo pipefail

readonly DAY="${1:-}"

if [[ -z "$DAY" ]]; then
  echo "Usage: $0 <day_number>"
  echo "Example: $0 02"
  exit 1
fi

create_file_if_missing() {
  local file="$1"
  local description="$2"

  if [[ ! -f "$file" ]]; then
    touch "$file"
    echo "✅ Created $description"
  else
    echo "⏭️  $description already exists, skipping"
  fi
}

# Create Day module if it doesn't exist
if [[ ! -f "src/Day${DAY}.hs" ]]; then
  cat > "src/Day${DAY}.hs" <<EOF
module Day${DAY} (part1, part2) where

part1 :: String -> Int
part1 = const 1

part2 :: String -> Int
part2 = const 2
EOF
  echo "✅ Created src/Day${DAY}.hs"
else
  echo "⏭️  src/Day${DAY}.hs already exists, skipping"
fi

# Create data files
mkdir -p data
create_file_if_missing "data/day_${DAY}_test.txt" "data/day_${DAY}_test.txt"
create_file_if_missing "data/day_${DAY}.txt" "data/day_${DAY}.txt"

# Add to cabal file if not already present
if ! grep -q "Day${DAY}" aoc2025.cabal; then
  awk -v day="Day${DAY}" '
    /^    Utils$/ {
      print "    " day
    }
    { print }
  ' aoc2025.cabal > aoc2025.cabal.tmp && mv aoc2025.cabal.tmp aoc2025.cabal
  echo "✅ Updated aoc2025.cabal"
else
  echo "⏭️  Day${DAY} already in aoc2025.cabal, skipping"
fi

# Update Main.hs to use the new day
cat > "src/Main.hs" <<EOF
import Day${DAY} (part1, part2)

main :: IO ()
main = do
  testData <- readFile "data/day_${DAY}_test.txt"
  realData <- readFile "data/day_${DAY}.txt"
  print \$ part1 testData
  print \$ part1 realData
  print \$ part2 testData
  print \$ part2 realData
EOF
echo "✅ Updated src/Main.hs to use Day${DAY}"

echo ""
echo "🎄 Day ${DAY} is ready!"
echo ""
echo "Next steps:"
echo "  1. Add test input to data/day_${DAY}_test.txt"
echo "  2. Add real input to data/day_${DAY}.txt"
echo "  3. Implement part1 and part2 in src/Day${DAY}.hs"
echo "  4. Run: cabal run"
