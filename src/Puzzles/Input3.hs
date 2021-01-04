module Puzzles.Input3 where

import           Data.Text

input :: [Text]
input =
  [ ".##.............##......#.....#"
  , ".#.#................#.........."
  , "...#..#.##..#.#......#.#.#.#..#"
  , "..#......#........#..#..#.#.#.."
  , ".......#....#..#..#.......#...."
  , "..#..#..##.#...#........#.###.."
  , "..#...#..#.....#.##....#......."
  , "....#..###.#......#.##..#...##."
  , "..#..........#.##.#...#........"
  , "#...#............##....#..##..."
  , ".......##....#.....##..#.#..#.#"
  , "..#..#..#...#....#....#....#..."
  , ".#...#.##........#####........#"
  , "..#..#......#.....##...#......."
  , "....#......##....#.#....#.#..##"
  , "#.#.##....##..#.........#.###.."
  , "##..###..#..#.......###.......#"
  , "...#.#......#.........#....#..."
  , ".....#..........#.....##..#.#.."
  , "....##......#.#..#....#.#......"
  , "..#.....#..##.......##......#.."
  , ".........##.##.#..##..........."
  , "....#...#.....#....#.#.###....#"
  , ".##.#..#...##..#.......#......#"
  , "##..#..#..####..#.#..#...#....."
  , "..###..#..#..#.###..#....#.##.."
  , "......#...###.#.#.....#........"
  , ".....#...#.#...#.......#.....#."
  , "#........#..##...........#..#.."
  , ".#.##.##...#.....#.#....#..#..."
  , "..##.##....#.....#....#....##.."
  , "#.........##...##..#.....#..#.."
  , "........#.####....#...##.....#."
  , ".#.#...#..#..#.#......##.....#."
  , "..#..........##..#.#.#....#...#"
  , "#.......#...#...#.....#.##.#..."
  , "..#.....#..#.....####.#..#.#.##"
  , "...#.#..#...#.....#...#.#.#.#.#"
  , ".#..##....##.....#..#....###..."
  , "....#......##.#.#.....#......#."
  , "..#.#...#......#.....##.......#"
  , "..#...###...#..#.#...#..#.....#"
  , "#..............#.....#....##..#"
  , ".#...#.......#.............#..."
  , "..###....#.##........#.#......."
  , "#.##.......#..#............###."
  , "#...#..##.#.#............######"
  , "..##..#....#.#.###...#..##.##.."
  , ".#...#.###.#....#...#....#...#."
  , "#...#.......#...........#...##."
  , "##.#......#####.............#.."
  , "....#..#......##..#..........#."
  , "#.....#.....#.#.......#...#...#"
  , "....#...#.#..##........#.#..##."
  , "..##.....##............#.#.###."
  , "#.........#........#..###......"
  , "............#.................."
  , ".#.###...####...#.#..#......#.."
  , "...##.###.#....##.#..####..#.##"
  , "..#####.#.##...#.#...##.##....#"
  , "........##...#...#....##.....##"
  , "#...........###...#.#...##.#..."
  , "##......#...#.......###........"
  , "..#..#.##.#..###....#..#.###.#."
  , "...#.#.#...#....#.##..#...#...."
  , "........#.##...##.#.....##...##"
  , ".#..........##..#..#..#.#...#.."
  , "#.#...#.##.#....#.##..#........"
  , ".#.#.#....##..##.####.....###.."
  , "..#....##....#..#..#..##......."
  , "..##...#.......#...##.#....#..."
  , "...####.#.#...........#.#...#.."
  , "....####.....#.#.....#....##.##"
  , "..#.....#.#.............##....#"
  , "#.#....#####.##..####.#...#.#.."
  , "#.#....#.##.#.#.##..#.#...#...."
  , "......#.......#.......#.....#.."
  , "..#.....#....###.###..#..#..#.."
  , "#..#....##.###...##.....#......"
  , "..#..#...#..#.##..........#...."
  , "...#.#.#......#....#.##..#..##."
  , "....##.#....#..#...##....###..."
  , "##.#.###.....#..#.#.#..#.....##"
  , "##..#.#........#...#..#.#......"
  , "....#.#.......##.#...........#."
  , ".......##...#...#...#.....#...."
  , ".....#....#..#..###.#...#......"
  , "............#.#..#......#.#...."
  , "...##..#.##....##..##.#......#."
  , "#.#.#......#.#.....#.#.#..#.#.#"
  , "...###..........#..#..#.##....."
  , "......#......#......###..##...."
  , "#...##...#....#....#..#...#.#.."
  , ".......#..#......##.......#...."
  , "...#..#..#.....#.....#......##."
  , "..#....###..........##..#...#.."
  , "..........#..#.#...#......#..#."
  , "#...#....#.##.........#.#.#...#"
  , ".#.#.#...#.#...#.#..#..#....#.#"
  , "#.##....#..#.........#.##.##..#"
  , "..#.#..##.#....#.###.#...#....#"
  , ".#.......#...#.#.........#....#"
  , ".......#...#..........#.#..#..."
  , "...#.....##..#....#...###...#.#"
  , "#....##.##..........#.......#.."
  , ".##..##......#...#....#.##....#"
  , "....#.....##...##.#..#........."
  , "...#.#..##.#.#..#.......#....#."
  , ".#...#.#.#.#..#..#.##.......#.."
  , "..#..##...#.#..#.......#.#####."
  , ".......#.#...........#....#.#.."
  , ".#.###..........#...#..#...#..."
  , "..#.#............##......##...."
  , "...##..#...###...##....#.#.##.."
  , "....#.##...#..#.#.#...........#"
  , "....#..#....##.....#.##.#.....#"
  , "..##......##.#.........#....#.#"
  , "###...#...#..#.#...#..........."
  , ".####.....#.....#.#....#..#...."
  , ".#....#..#..#..#...##.....###.#"
  , "#....##.#....#.##..#......##..#"
  , ".........#..#.#.....#.#....##.#"
  , ".....#.#...#....#.#...#....#..#"
  , ".#...#.#.....#.#......#.#......"
  , "#....##.......#.............#.."
  , "#..#...#........##..#..#......#"
  , "####..##.#..##..#.........####."
  , ".#.##..#.#..#.#.#.##...#..#.#.#"
  , ".##.#..#............#......#..."
  , "###....#.##....#..#...#........"
  , ".....#..###..........#..#......"
  , ".##..##.....#..##....#...#....."
  , "#...##...........#..#...###..#."
  , "#..##....#...#.##.##....#......"
  , "...#...#..#.#.......##.......##"
  , "....#.....#..#...#.........#.#."
  , ".#...##.#......#.#..#..#...##.."
  , "...##...##.##...##...#..#......"
  , "#..##.#..#..#............#...#."
  , "..#.....#.........#........#.#."
  , "#.#...#...#......#.#......#...."
  , ".##.....##.#.#....#.##...##.#.."
  , ".##..##.#.#....#.##............"
  , ".#.##.....##.#...#...###....#.."
  , ".#..............#.#....###.###."
  , "....#..#...#.#.#..........#.#.#"
  , ".#.#..#.#.#...###..#...##......"
  , ".#.#.....###......#..........#."
  , "........#.##...............#.#."
  , "...#.#.#......#..#..##........#"
  , "..#.##......#.......#..#......#"
  , "...#...#...#...#..#..#........#"
  , "..#....#.....#....#..##........"
  , ".....#..#...##....#......##...."
  , "...##..##..#..........##....#.#"
  , "..#....#..#...#.##..#.....##..."
  , "###...#.#....#........#.......#"
  , "......#...#..#....###.........#"
  , "..###.#...#...#...#.#..###.#..."
  , ".##.#.......#.#..#..#......#.#."
  , "...##...........#.#..#.#..#...."
  , ".......#.....####.#.....#...#.#"
  , "......##......##.#.#.#...#.#..#"
  , "..###.#####..#....#.#...#..##.."
  , ".....#..#......#........#......"
  , "#..##....#.#.##....#....#......"
  , ".#....#.##.####.##..#..#......#"
  , "#...##...#.#...##.#.##.##...#.."
  , "........#.#........#.#...#..#.."
  , ".#....###..#......#.##.###..#.."
  , ".#..#..#..#..#...#.#.........##"
  , "....#...#..#..............#...."
  , "........#...##.....#.......#..."
  , "..#......#.#..#.#..#.#.#...###."
  , "....#...####....###....#......#"
  , "#...#.#...................#.##."
  , "..#.#.###...#....##....##......"
  , "#..##..#.........#....#....####"
  , ".#....###...#.#...#......#...#."
  , "......#..#.#..#.##...#.#.#..#.."
  , ".#...#.#.....#..##......#..#..."
  , "##.#..##.....##.#.#.......##..."
  , ".##.##.##..#...#.#.##.##......."
  , ".#.#......#.....#...#.#..#....."
  , "...#...........#..#.##..##..#.."
  , ".....#...##......#........#.#.."
  , "....#..............##.........#"
  , "..####.#....##..##......##.#.#."
  , ".#.#..#...#..........#...###..#"
  , "....#.#.##.....###.#..#.##....."
  , ".......##.#.#..#...#...##.#...."
  , "...#.##.....#....#...#...#....."
  , "##.#.##..#..#.#.....#.#..#....."
  , "..#..##........#....###..#..#.."
  , "..#.........##.....#......#...#"
  , "...##..........##......#.#.#..."
  , "#.....#..#..#......#......#...."
  , ".##...#..##....#.......##..#.#."
  , ".#...##...##......####.##.#...."
  , ".....#.........#.#.####......#."
  , "...#.....#.#.........##..#....."
  , "##.#.###.#..#.#..#............#"
  , "...##..#.#....#....#..#........"
  , "..#.###......#...#.#.....#...#."
  , "....##.##..#.....#...#.#.#....#"
  , ".......#.#..#...........#.#...."
  , ".#.#..##.#.......#.#..#.....###"
  , "...#.#.....#.#..#.##..#...#.#.."
  , "...#......##....#.............."
  , "......#...#...................."
  , "..#........#...##.##.#..#.#.#.."
  , ".#.###.#.##..##..#....##....#.."
  , ".....#..#.#...#.#..#..#.......#"
  , "..........#.##.#..##..####....."
  , "............#.#......#........."
  , ".#....#..#......#.....##......."
  , ".....#........#.....##.#..#.#.."
  , "#..#.##...#.#.....#...#.####..."
  , "......#...#....#.##..##.#...#.."
  , ".#.#.##......##....#.#....#.##."
  , "#.#.#....#.###....##....##....."
  , ".##..#...#.##......#..#..#...##"
  , "...#....###....#...........#.#."
  , "#.#.##.##...##....#....##.#...#"
  , ".#.#######.......#......#......"
  , "#......#...#.#.#.###....#.##..#"
  , "......##..#..##.........##.#.##"
  , "....##...#.#....##.....#.....#."
  , "..#.#........##........#.#..##."
  , ".....#..#.##.....#.....#..#.#.."
  , ".#..............#.......#......"
  , ".............#..#..........#..."
  , ".#..#.##....##.#..#...##......."
  , "...........#..#.......#.#....#."
  , ".#..#..........##...#.#.#...#.."
  , "......#....#..###....#......#.."
  , ".#...#...##..#..#..##..#..#.#.."
  , "#.#.........#....#..........##."
  , "...##..#..##...#....##...##.##."
  , "..#....#.####.........#.....##."
  , ".....#.#...#.#...#.##.#...##..#"
  , "#...#.....#..#.......#...#..#.."
  , "..#.......#..##.#.....#....#..."
  , ".#.....#..##.#.....#...#.#...#."
  , ".....#.##..........##....#...#."
  , "...#....#...#........##...#...#"
  , "....##...#....#..........#....."
  , "...#....##..#..####..##.#...#.#"
  , "#...###.###..#....##.#........."
  , ".#.......#......#.........#...."
  , "..#..##..#.........##.........."
  , "#......#.#.##...#...#####......"
  , "......#.....####......#....#..."
  , ".........#..#..#...#....#.#...."
  , "....#........#...##....#......."
  , "...##.#...#..........#....#...."
  , "........#.......#.##..#..#...#."
  , "....#..##...........#.....#..#."
  , "#....#...............#.#....#.."
  , ".#........#....#.#...#.......#."
  , "#.......##..........#.......#.."
  , "...#....#...##.#..#.......#...."
  , "#..#.##...#.#...#...#...#....#."
  , "###...#...#....#....#....#...#."
  , "##......#.#.......#....#..#...."
  , "......#......#....#.#.#..###..#"
  , ".#.#.##.....#..#..........#...."
  , "##...#.#.#..##....#.....#.#...."
  , "#.##...#...#.#...####..#......."
  , ".....##..#.#.#....#..##..#.#..."
  , "....###.#.........##.....#....."
  , "......##...........#........#.#"
  , ".#.........##.................."
  , ".........##...#.............#.#"
  , "......##...#...#.........#..##."
  , "#..#.......#..##.......###....."
  , "....#.#.....#............##...."
  , ".....#..#......#....#.....##..."
  , "##......##...................#."
  , "#....#............#.#.###.##..."
  , ".#.....#........#.....#...#...."
  , "......##.......######......##.."
  , ".#....##....#..###....#.......#"
  , "..............##.#..#.......#.#"
  , ".#..#..........#..#.##........."
  , "......##.#..#......#.#....##.#."
  , "#.....#.##...#.....#...#..#...#"
  , ".#....#..##.....#.....#.#.#...."
  , "..#......#.##..#.........#.#.#."
  , ".#..##...#...#.....#..#..#.#..#"
  , "#.#.##.##.................#.#.#"
  , ".#..#.#..##.#.......#.......##."
  , "#...#...#..##...#...##...#...#."
  , "....#......#..#...#.....##..#.."
  , "..............##......#...#.#.."
  , "..##..#.......#..#..###.#.#...."
  , ".#..#..#...#.......#...#...##.#"
  , ".#...#.......###..#.##.###....."
  , "##.#...#......#.....#..#......."
  , "##....##............#.....#..#."
  , ".....#...##......##.....#....##"
  , "#...##..#....#..##....###.#...#"
  , ".....#..#.#.....#.##..##....#.."
  , ".#.....#.#........#...#.#......"
  , "......#....#.#........#.#......"
  , ".##..#...............###...##.#"
  , ".......###.#.#......###.....#.."
  , ".......#..##...#....#.##..#.##."
  , "..#.......##.......#.....#....#"
  , ".#......#....#..##..#.#.#..##.."
  , "###......#...#..#.............#"
  , ".#....#..#.#......##..........."
  , ".#....#.##.....#..#.......#..##"
  , "....#...#...#..#.....#..##..#.#"
  , "#.#.#.......##.#..#.#....#....."
  , "##.#.......#...#...#.#......##."
  , "#....#.#...........#######....."
  , "...#.#.##.#......##..###......."
  , "..#.#....#..#.................#"
  , "........#..##..#.....#....#.##."
  , "...#.#..#..#..#..............##"
  , ".##.......###.#......#....#..##"
  , "..##.##.#......#....#..#...#..#"]
