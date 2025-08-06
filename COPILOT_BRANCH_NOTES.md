# Copilot-Augmented Tournament Branch Documentation

## Overview
This branch (`copilot-augmented-tournament`) contains major enhancements to the Texas Hold'em simulation, focusing on interactive tournament play, betting logic, and graphical output. All code is in `R/poker.R` as requested.

## Key Features
- **Interactive Tournament Mode**: Play multiple hands with named players, dealer button, blinds, and chip stacks.
- **Modular Betting System**: Handles blinds, betting order, repeated betting rounds, all-in, sidepots, and correct player actions.
- **Improved Graphics**: The `cgiPlayers` function now displays dealer, chip counts, current bets, and pot/sidepot(s) after every action.
- **User-Friendly Prompts**: Betting actions use single-key input and correct terminology (check, call, raise, fold).
- **Roxygen Documentation**: All new functions are documented in detail, matching the style of the original code.

## Known Issues & Unimplemented Goals
- **Sidepot Handling**: Basic support is present, but complex multi-sidepot scenarios are not fully implemented.
- **All-In Logic**: Players who are all-in are not prompted for further actions, but edge cases may need refinement.
- **Graphics**: Pot and bet display is textual; graphical chips/pot visualization could be improved.
- **Hand Evaluation**: Winner determination uses existing logic; split pots and ties are handled, but edge cases may need more testing.
- **Error Handling**: User input is validated, but more robust error handling could be added.

## How to Use
- Switch to this branch: `git checkout copilot-augmented-tournament`
- Run the interactive tournament: `interactive_tournament(c("Alice", "Bob", "Carol"), chips=1000)`
- All code is in `R/poker.R`. No other files are required.

## Future Improvements
- Full sidepot logic for multi-way all-in scenarios
- Enhanced graphics for chips and pot
- Tournament summary and hand history
- AI/bot players for solo play
- More robust error handling and edge case coverage

---
This file documents all major changes, features, and known issues for the Copilot-augmented branch. For questions or suggestions, open an issue or contact the branch maintainer.
