# Questionnaire App

Started out as a simple [gist](https://gist.github.com/omer-biz/53166306844e8f9347fd416a6b2349e8).

This is a simple questionnaire application implemented in Elm. The application
allows users to navigate through a list of questions, answer them, and manage
the history of questions. Additionally, the app provides a notification system
for various actions.

## Features

- **Question Navigation:** Users can navigate through a list of questions using
  "Next" and "Previous" buttons.
- **Question Deletion:** Users can delete the current question. If it's the
  last question, deletion is restricted.
- **Undo Deleted Question:** Users can undo the deletion of a question.
- **Notification System:** The app provides notifications for actions such as
  saving, deleting, and undoing.

## Model

The `Model` represents the state of the application and consists of:

- `status`: A list of status messages with timers for notifications.
- `questions`: A history object that includes the current question, a list of
  previous questions, and a list of upcoming questions.

## Messages

The `Msg` type defines various actions that can be performed in the
application, such as changing a question, saving a question, navigating to the
next or previous question, and handling timers.

## Initialization

The `init` function initializes the application with an initial model and no
commands.

## Update

The `update` function handles messages and updates the model accordingly.
Actions include saving questions, navigating through the questionnaire,
handling timers for notifications, and more.

## View

The `view` function defines how the application should be displayed. It
includes components for showing notifications, displaying the current question
and answer input, and providing navigation buttons.

## Subscriptions

The `subscriptions` function manages time-based subscriptions, specifically for
updating the timer on notifications.

## Main

The `main` function sets up the Elm program for a browser element, specifying
the initialization, view, update, and subscriptions.

## Style

The application includes a set of styles for buttons, input fields,
notifications, and page navigation, enhancing the user interface.

## Color

A set of predefined colors is provided for styling the application.

Feel free to extend and modify this Elm codebase to suit your specific needs or
integrate it into a larger project.
