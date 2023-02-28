-- to start, open up terminal and type:
-- ghci
-- :load todolist
-- main

import System.IO
import System.Directory

-- File name
fileName = "tasks.txt"

-- Main loop
main = do {
  putStrLn "This is a todo list app";
  putStrLn "Do you want to continue? (y/n)";
  yn <- getLine;
  continue yn
  }

-- Processing 
continue ans
  | ans == "y" = do
      tasks <- loadTasks
      showCurrentTasks tasks
  | ans == "n" = putStrLn "Aborting..."
  | otherwise = do {
    putStrLn "Please enter y or n only!";
    putStrLn "Do you want to continue? (y/n)";
    yn <- getLine;
    continue yn
    }

-- Load Tasks from File
loadTasks :: IO [String]
loadTasks = do
  fileExists <- doesFileExist fileName
  -- check if file exists
  if fileExists
    then do
      -- get contents
      contents <- readFile fileName
      -- return contents as array, each new line acting as separator
      return (lines contents)
    else return []

-- Get current tasks for user
showCurrentTasks :: [String] -> IO ()
showCurrentTasks tasks = do {
  -- Print preamble
  putStrLn "This is your current todo list";
  putStrLn "------------------------------";
  -- Print tasks 
  printTasks tasks 0;
  -- Print prompts
  putStrLn "To add a task, enter a";
  putStrLn "To remove a task, enter r";
  putStrLn "To remove ALL tasks, enter x";
  putStrLn "To quit, enter q";
  -- Get input
  res <- getLine;
  -- Process
  handleInput tasks res
}

-- Goes through task list and prints each task with its index, due date in different color according to how urgent it is.
printTasks :: [String] -> Int -> IO ()
printTasks [] _ = return ()
printTasks (task:tasks) index = do {
  putStrLn ((getColorByUrgent (head task)) ++ show index ++ ". " ++ (tail task) ++ "\x1b[0m");
  printTasks tasks (index + 1)
}

-- gets textColor by urgentNo
getColorByUrgent :: Char -> [Char]
getColorByUrgent urgentNo
  -- green
  | urgentNo == '1' = "\x1b[32m"
  -- yellow
  | urgentNo == '2' = "\x1b[33m"
  -- red
  | urgentNo == '3' = "\x1b[31m"
  -- k = 0, basic color
  | otherwise = "\x1b[0m"

-- Add tasks to task list and update file
addTask :: [String] -> IO ()
addTask tasks = do
  putStrLn "Enter Task"
  task <- getLine
  -- get task date
  putStrLn "Enter due year eg.2023 or 2024";
  dueYear <- getLine
  putStrLn "Enter due month eg. 1/2/3/.../12";
  dueMonth <- getLine
  putStrLn "Enter due day eg. 1/2/3/..../31, make sure it matches the month (no 30th in Feb)"
  dueDay <- getLine
  -- check task date
  if (not (isDateValid (read dueDay) (read dueMonth) (read dueYear)))
    then do
      -- if failed check, start again
      putStrLn "invalid date"
      addTask tasks
    else do
      putStrLn "How urgent is this? Enter 0 to 3, 3 being most urgent"
      urgentNo <- getLine
      -- check priority number is 0 to 3
      if (not (urgencyCheck urgentNo))
      then do
        -- if failed check, start again
        putStrLn "invalid piority number"
        addTask tasks
      else do
        -- add to tasks if checks pass
        let newTasks = tasks++[urgentNo ++ task ++ ", due by " ++ dueDay ++ "/" ++ dueMonth ++ "/" ++ dueYear]
        saveTasks newTasks
        putStrLn "Task added!"
        putStrLn "going back to main page..."
        showCurrentTasks newTasks

-- checks if urgentNo is between 0 to 3
urgencyCheck :: [Char] -> Bool
urgencyCheck urgentNo
  | (read urgentNo < 4 && read urgentNo >= 0) = True
  | otherwise = False

-- checks if the date user provided is valid
isDateValid :: Int -> Int -> Int -> Bool
isDateValid d m y
  | y < 2023                       = False
  | m `elem` [1,3,5,7,8,10,12]     = d <= 31
  | m `elem` [4,6,9,11]            = d <= 30
  | m == 2                         = d <= if isLeapYear y then 29 else 28
  | otherwise                      = False
  where isLeapYear y = (y `mod` 4 == 0) && (y `mod` 100 /= 0 || y `mod` 400 == 0)

-- Remove task using index number
removeTask :: [String] -> IO ()
removeTask tasks = do 
  -- Ask for task number
  putStrLn "Enter the Number of the Task to Remove";
  -- Get input
  taskNumInput <- getLine
  -- Turn into Int
  let taskNum = read taskNumInput :: Int
  -- Handle out of bounds
  if (taskNum >= length tasks || taskNum < 0) then do {
    putStrLn "Invalid Task Number!";
    removeTask tasks;
  } else do 
    -- Get new task list without removed task
    let newTasks = take taskNum tasks ++ drop (taskNum+1) tasks;
    -- Save it
    saveTasks newTasks;
    putStrLn "Task Successfully Removed"
    putStrLn "Going Back to Main Page"
    -- Go back to main
    showCurrentTasks newTasks;
  
-- Remove all tasks from todo
removeAllTasks :: IO ()
removeAllTasks = do {
  -- Double check with User
  putStrLn "Are you sure you want to delete ALL tasks? (y/n)";
  -- Get input
  answer <- getLine;
  -- Handle Yes
  if (answer == "y") then do {
    -- Write empty string to file
    writeFile fileName "";
    putStrLn "All Tasks Successfully Removed!";
    showCurrentTasks [];
  -- Handle No
  } else if (answer == "n") then do {
    -- Go back
    showCurrentTasks []
  -- Handle invalid input
  } else do {
    putStrLn "Please enter either 'y' or 'n' only!";
    removeAllTasks
  }
}

-- Handle inputs 
handleInput :: [String] -> String -> IO ()
handleInput tasks res
  | res == "a" = addTask tasks
  | res == "r" = removeTask tasks
  | res == "x" = removeAllTasks
  | res == "q" = putStrLn "Aborting..."
  | otherwise = do {
    putStrLn "Please enter a,r,x or q only!";
    res <- getLine;
    handleInput tasks res
  }

saveTasks :: [String] -> IO ()
saveTasks tasks = writeFile fileName (unlines tasks);