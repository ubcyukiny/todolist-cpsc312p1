-- to start, open up terminal and type:
-- ghci
-- :load todolist
-- main

import System.IO
import System.Directory

fileName = "tasks.txt"


main = do {
  putStrLn "This is a todo list app";
  putStrLn "Do you want to continue? (y/n)";
  yn <- getLine;
  continue yn
  }

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

loadTasks :: IO [String]
loadTasks = do
  fileExists <- doesFileExist fileName
  if fileExists
    then do
      contents <- readFile fileName
      return (lines contents)
    else return []

showCurrentTasks :: [String] -> IO ()
showCurrentTasks tasks = do {
  putStrLn "This is your current todo list";
  putStrLn "------------------------------";
  printTasks tasks 0;
  putStrLn "To add a task, enter a";
  putStrLn "To quit, enter q";
  res <- getLine;
  addTaskOrQuit tasks res
}

printTasks :: [String] -> Int -> IO ()
printTasks [] _ = return ()
printTasks (task:tasks) index = do {
  putStrLn (show index ++ ". " ++ task);
  printTasks tasks (index + 1)
}

addTask :: [String] -> IO ()
addTask tasks = do 
  putStrLn "Enter Task"
  task <- getLine
  let newTasks = tasks++[task]
  saveTasks newTasks
  putStrLn "Task added!"
  putStrLn "going back to main page..."
  showCurrentTasks newTasks


addTaskOrQuit :: [String] -> String -> IO ()
addTaskOrQuit tasks res
  | res == "a" = addTask tasks
  | res == "q" = putStrLn "Aborting..."
  | otherwise = do {
    putStrLn "Please enter a or q only!";
    res <- getLine;
    addTaskOrQuit tasks res
  }

saveTasks :: [String] -> IO ()
saveTasks tasks = writeFile fileName (unlines tasks);