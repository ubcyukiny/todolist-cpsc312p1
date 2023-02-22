-- to start, open up terminal and type:
-- ghci
-- :load todolist
-- main

main = do {
  putStrLn "This is a todo list app";
  putStrLn "Do you want to continue? (y/n)";
  yn <- getLine;
  continue yn
  }

continue ans
  | ans == "y" = showCurrentTasks
  | ans == "n" = putStrLn "Aborting..."
  | otherwise = do {
    putStrLn "Please enter y or n only!";
    putStrLn "Do you want to continue? (y/n)";
    yn <- getLine;
    continue yn
    }

showCurrentTasks = do {
  putStrLn "This is your current todo list";
  putStrLn "------------------------------";
  -- display current todo tasks
  putStrLn "To add a task, enter a";
  putStrLn "To quit, enter q";
  res <- getLine;
  addTaskOrQuit res
}

addTask = do {
  -- add task function here
  putStrLn "Task added!";
  putStrLn "going back to main page...";
  showCurrentTasks
}

addTaskOrQuit res
  | res == "a" = addTask
  | res == "q" = putStrLn "Aborting..."
  | otherwise = do {
    putStrLn "Please enter a or q only!";
    res <- getLine;
    addTaskOrQuit res
  }
