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
  putStrLn "To quit, enter q";
  -- Get input
  res <- getLine;
  -- Process
  handleInput tasks res
}

-- Goes through task list and prints each task with its index.
printTasks :: [String] -> Int -> IO ()
printTasks [] _ = return ()
printTasks (task:tasks) index = do {
  putStrLn (show index ++ ". " ++ task);
  printTasks tasks (index + 1)
}

-- Add tasks to task list and update file
addTask :: [String] -> IO ()
addTask tasks = do 
  putStrLn "Enter Task"
  task <- getLine
  let newTasks = tasks++[task]
  saveTasks newTasks
  putStrLn "Task added!"
  putStrLn "going back to main page..."
  showCurrentTasks newTasks

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
  
-- Handle inputs 
handleInput :: [String] -> String -> IO ()
handleInput tasks res
  | res == "a" = addTask tasks
  | res == "r" = removeTask tasks
  | res == "q" = putStrLn "Aborting..."
  | otherwise = do {
    putStrLn "Please enter a or q only!";
    res <- getLine;
    handleInput tasks res
  }

saveTasks :: [String] -> IO ()
saveTasks tasks = writeFile fileName (unlines tasks);