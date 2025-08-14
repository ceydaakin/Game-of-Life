-- \| This Haskell program simulates Conway's Game of Life, a zero-player game,
-- \| and focuses specifically on generating and visualizing the evolution of
-- \| five common "oscillator" patterns. Oscillators are configurations that
-- \| return to their original state after a fixed number of generations (their period).
-- \|
-- \| The program generates a sequence of PNG image files for each oscillator,
-- \| showing its state through one full period of its evolution.
-- \| Each cell is represented by a pixel block in the output image.

-- Import required libraries for image generation and directory handling
import Codec.Picture -- For creating and saving PNG images
import System.Directory (createDirectoryIfMissing) -- For creating output folders

-- | Defines the size of the square grid for the Game of Life simulation.
-- | A 50x50 grid provides enough space for the patterns to evolve.
gridSize :: Int
gridSize = 50

-- | Defines the pixel size of each individual cell when rendered into an image.
-- | A 10x10 pixel cell makes the patterns clearly visible.
cellSize :: Int
cellSize = 10

-- | Calculates the total resolution of the output image based on grid size and cell size.
-- | (50 cells * 10 pixels/cell = 500 pixels total)
imageSize :: Int
imageSize = gridSize * cellSize

-- | Converts an integer cell value (0 for dead, 1 for alive) into an RGB color.
-- | This function determines the visual representation of each cell.
-- | '0' (Dead): White (RGB: 255, 255, 255)
-- | '1' (Alive): Blue (RGB: 0, 0, 255)
-- | Any other value: Black (Error state, though not expected in this simulation)
colorFor :: Int -> PixelRGB8
colorFor 0 = PixelRGB8 255 255 255 -- white (dead cell)
colorFor 1 = PixelRGB8 0 0 255 -- blue (alive cell)
colorFor _ = PixelRGB8 0 0 0 -- black (error/unexpected cell value)

-- | Calculates the top-left (x, y) coordinates to center a smaller pattern
-- | within the larger 'gridSize' x 'gridSize' simulation grid.
-- | 'w' is the width of the small pattern.
-- | 'h' is the height of the small pattern.
center :: Int -> Int -> (Int, Int)
center w h = ((gridSize - w) `div` 2, (gridSize - h) `div` 2)

-- | Embeds a given small 2D pattern (represented as a list of lists of Ints)
-- | into the full simulation grid at a specified top-left position (x0, y0).
-- | Cells outside the small pattern area are initialized as dead (0).
placePattern :: (Int, Int) -> [[Int]] -> [[Int]]
placePattern (x0, y0) small =
  [ [ if x >= x0 && x < x0 + w && y >= y0 && y < y0 + h
        then (small !! (y - y0)) !! (x - x0) -- Get value from small pattern
        else 0 -- Default to dead if outside pattern bounds
      | x <- [0 .. gridSize - 1] -- Iterate through columns of the main grid
    ]
    | y <- [0 .. gridSize - 1] -- Iterate through rows of the main grid
  ]
  where
    h = length small -- Height of the small pattern
    w = length (head small) -- Width of the small pattern

-- | Computes the next generation of the Game of Life grid based on the current state.
-- | This function applies Conway's rules to every cell in the grid simultaneously.
nextGen :: [[Int]] -> [[Int]]
nextGen grid =
  -- Create a new grid by applying 'nextCell' to each coordinate (x,y)
  [[nextCell x y | x <- [0 .. gridSize - 1]] | y <- [0 .. gridSize - 1]]
  where
    -- \| Determines the state of a single cell (x,y) in the next generation.
    nextCell x y =
      let alive = grid !! y !! x -- Current state of the cell (1 if alive, 0 if dead)
          neighbors = countLiveNeighbors x y -- Number of live neighbors for the cell
       in case (alive, neighbors) of
            -- Rule 1: A living cell with exactly 2 or 3 living neighbors remains alive.
            (1, 2) -> 1
            -- Rule 2: An dead cell with exactly 3 living neighbors becomes alive (birth).
            (_, 3) -> 1 -- (Can be a living or dead cell, if 3 neighbors, it lives/is born)
            -- Rule 3: All other living cells die (underpopulation < 2, overpopulation > 3).
            -- Rule 4: All other dead cells remain dead.
            _ -> 0 -- (Dead if less than 2, more than 3 living neighbors, or not 3 neighbors for a dead cell)

    -- \| Counts the number of living neighbors for a given cell (x,y).
    -- \| It iterates through all 8 adjacent cells (including diagonals).
    countLiveNeighbors x y =
      length
        [ () -- We just count the occurrences, so '()' is a placeholder
          | dx <- [-1 .. 1], -- Delta x: -1 (left), 0 (current column), 1 (right)
            dy <- [-1 .. 1], -- Delta y: -1 (up), 0 (current row), 1 (down)
            (dx, dy) /= (0, 0), -- Exclude the cell itself (center, dx=0, dy=0)
            -- Corrected: Use 'let' for multiple local bindings, separated by semicolons
            let nx = x + dx; ny = y + dy, -- Calculate neighbor's x and y coordinates
            nx >= 0,
            ny >= 0, -- Ensure neighbor is within grid bounds (top/left)
            nx < gridSize,
            ny < gridSize, -- Ensure neighbor is within grid bounds (bottom/right)
            grid !! ny !! nx == 1 -- Check if the neighbor cell is alive (value is 1)
        ]

-- | Defines five different well-known oscillator patterns in Conway's Game of Life.
-- | Each function takes an Int argument (unused here, but common for pattern families)
-- | and returns the initial configuration of the pattern as a 2D list of Ints.

-- | Pattern 1: Blinker (Period 2)
-- | A simple 3-cell line that oscillates horizontally and vertically.
-- | [1,1,1]
pattern1 :: Int -> [[Int]]
pattern1 _ = placePattern (center 3 1) [[1, 1, 1]]

-- | Pattern 2: Toad (Period 2)
-- | Two small blocks of 3 cells each that "breathe" back and forth.
-- | [0,1,1,1]
-- | [1,1,1,0]
pattern2 :: Int -> [[Int]]
pattern2 _ =
  placePattern
    (center 4 2)
    [ [0, 1, 1, 1],
      [1, 1, 1, 0]
    ]

-- | Pattern 3: Beacon (Period 2)
-- | A 4x4 block with two active diagonal corners that toggle.
-- | [1,1,0,0]
-- | [1,1,0,0]
-- | [0,0,1,1]
-- | [0,0,1,1]
pattern3 :: Int -> [[Int]]
pattern3 _ =
  placePattern
    (center 4 4)
    [ [1, 1, 0, 0],
      [1, 1, 0, 0],
      [0, 0, 1, 1],
      [0, 0, 1, 1]
    ]

-- | Pattern 4: Pulsar (Period 3)
-- | A large, symmetrical pattern that expands and contracts in three phases.
-- | It has a very distinctive "pulsing" appearance.
pattern4 :: Int -> [[Int]]
pattern4 _ =
  placePattern
    (center 13 13)
    [ [0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0],
      replicate 13 0, -- A row of 13 dead cells
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1],
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1],
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1],
      [0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0],
      replicate 13 0,
      [0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0],
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1],
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1],
      [1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1],
      replicate 13 0,
      [0, 0, 1, 1, 1, 0, 0, 0, 1, 1, 1, 0, 0]
    ]

-- | Pattern 5: Pentadecathlon (Period 15)
-- | A long, thin pattern that oscillates with a relatively long period,
-- | featuring two "flashing" blocks at its ends.
-- | [0,0,1,0,0,0,0,1,0,0]
-- | [1,1,0,1,1,1,1,0,1,1]
-- | [0,0,1,0,0,0,0,1,0,0]
pattern5 :: Int -> [[Int]]
pattern5 _ =
  placePattern
    (center 10 3)
    [ [0, 0, 1, 0, 0, 0, 0, 1, 0, 0],
      [1, 1, 0, 1, 1, 1, 1, 0, 1, 1],
      [0, 0, 1, 0, 0, 0, 0, 1, 0, 0]
    ]

-- | Converts the 2D integer matrix representing the Game of Life grid state
-- | into a 'PixelRGB8' image suitable for saving as a PNG.
-- | It maps each cell in the grid to a block of pixels in the image using 'cellSize'.
matrixToImage :: [[Int]] -> Image PixelRGB8
matrixToImage mat = generateImage getCellPixel imageSize imageSize
  where
    -- \| Determines the color of a specific pixel (x,y) in the output image.
    -- \| It figures out which grid cell the pixel belongs to and uses its value.
    getCellPixel x y =
      let row = y `div` cellSize -- Calculate the grid row index from pixel y
          col = x `div` cellSize -- Calculate the grid column index from pixel x
       in colorFor $ (mat !! row) !! col -- Get the cell value and convert it to color

-- | The main entry point of the program.
-- | It orchestrates the simulation, generation, and saving of images for each oscillator.
main :: IO ()
main = do
  -- Define the list of oscillator pattern functions
  let patterns = [pattern1, pattern2, pattern3, pattern4, pattern5]
      -- Define the period for each corresponding oscillator pattern
      periods = [2, 2, 2, 3, 15]

  -- Iterate through each pattern, its period, and an index (1 to 5)
  mapM_
    ( \(i, patFn, periodVal) -> do
        -- Create a unique directory name for each pattern's frames (e.g., "frames1", "frames2")
        let dirName = "frames" ++ show (i :: Int) -- 'i :: Int' explicitly types 'i' as Int
        createDirectoryIfMissing True dirName -- Create the directory if it doesn't exist
        putStrLn $ "Folder Created: " ++ dirName -- Confirm folder creation

        -- Calculate total frames to generate: period + 1 (to include initial state)
        let totalFrames = periodVal + 1
        -- Generate the sequence of grid states (generations) for the current pattern
        -- 'iterate nextGen (patFn 0)' creates an infinite list of generations
        -- 'take totalFrames' limits it to the necessary number of frames for one period
        let generations = take totalFrames $ iterate nextGen (patFn 0)

        -- For each generation (matrix 'mat') and its frame index 'j':
        mapM_
          ( \(j, mat) -> do
              let image = matrixToImage mat -- Convert the grid state to an image
              let fileName = dirName ++ "/frame" ++ show (j :: Int) ++ ".png" -- Construct the filename (e.g., "frames1/frame0.png")
              savePngImage fileName (ImageRGB8 image) -- Save the image as a PNG file
              putStrLn $ "Saved: " ++ fileName -- Confirm file saving
          )
          (zip ([0 ..] :: [Int]) generations) -- '([0..] :: [Int])' explicitly types list of Ints for 'j'
    )
    (zip3 ([1 ..] :: [Int]) patterns periods) -- '([1..] :: [Int])' explicitly types list of Ints for 'i'
