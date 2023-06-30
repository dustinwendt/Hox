{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module GameDecl where

import           Colors
import           ComplexTypes                  hiding (name)
import           Control.Monad                 (void)
import           Data.Map                      hiding (update)
import           Data.Text                     hiding (snoc)
import           Data.Vector                   hiding (update, (!), (++))
import qualified Data.Vector.Mutable           as MVector
import           GI.Gtk                        hiding (main, on, (:=))
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Objects.Widget
import           GI.Gtk.Objects.Window
import           Util

data Event = Pass | Quit

-- :: IO GameState
mainDecl = run App {
  view = view'
  , update = update'
  , inputs = []
  , initialState = defaultGameState
                 }

view' :: GameState -> AppView Window Event
view' s = bin
  Window
  [#title := "Hox"
  , on #deleteEvent (const (True, Quit))]
  $ container Box
    [#orientation := OrientationVertical]
    [lifeLabel, activePlayerLabel, passesLabel, phaseLabel]
  where
    p = _players s ! You
    -- infoBox = container Grid [][p_life]
    lifeLabel = widget Label [#label := showT ("Life: " ++ show (_life p))]
    activePlayerLabel = widget Label [#label := showT ("Active Player: " ++ show (_activePlayer s))]
    passesLabel = widget Label [#label := showT ("Passes: " ++ show (_passes s)) ]
    phaseLabel  = widget Label [#label := showT ("Phase: " ++ show (_phases s)) ]

update' :: GameState -> Event -> Transition GameState Event
update' _ Quit = Exit
update' s Pass = Exit

-- List Example

data Todo = Todo {name :: Text, completed :: Bool}

data Event' = TodoTextChanged Text
  | TodoSubmitted | TodoToggled Int | Closed

data State = State { todos       :: Vector Todo
                   , currentText :: Text}

main' = void $ run App
  { view = view''
  , update = update''
  , inputs = []
  , initialState = State {todos = mempty, currentText = mempty}}

view'' :: State -> AppView Window Event'
view'' s = bin
  Window
  [#title := "TodoGtk+", on #deleteEvent (const (True, Closed))]
  (container Box
             [#orientation := OrientationVertical]
             [todoList, newTodoForm]
  )
  where
    todoList = BoxChild defaultBoxChildProperties { expand = True, fill = True}
               $ container Box
                         [#orientation := OrientationVertical]
                         (imap todoItem (todos s))
    todoItem i todo = bin CheckButton [#active := completed todo
                                      , on #toggled (TodoToggled i)] $
      widget Label [#label := completedMarkup todo, #useMarkup := True, #halign := AlignStart]
    newTodoForm = widget Entry [ #text := currentText s
                               , #placeholderText := "What needs to be done?"
                               , onM #changed (fmap TodoTextChanged . entryGetText)
                               , on #activate TodoSubmitted]
    completedMarkup todo
      | completed todo = "<s>" <> name todo <> "</s>"
      | otherwise      = name todo

update'' :: State -> Event' -> Transition State Event'
update'' s e = case e of
  TodoTextChanged t -> Transition s { currentText = t} (pure Nothing)
  TodoSubmitted ->
    let newTodo = Todo {name = currentText s, completed = False}
    in Transition
       s { todos = todos s `snoc` newTodo, currentText = mempty } (pure Nothing)
  TodoToggled i -> Transition s { todos = mapAt i toggleComplete (todos s) } (pure Nothing)
  Closed            -> Exit

toggleComplete :: Todo -> Todo
toggleComplete todo = todo { completed = not (completed todo)}

mapAt :: Int -> (a -> a) -> Vector a -> Vector a
mapAt i f = modify (\v -> MVector.write v i . f =<< MVector.read v i)

