# Nested Threshold Public Goods Game

This is the code for a behavioural experiment where people play nested threshold public goods games. 

There are three experimental conditions:
- Balanced. Set `C.GLOBAL_REWARD=2` in `game/__init__.py`.
- GlobalBoost. Set `C.GLOBAL_REWARD=2.5` in `game/__init__.py`.
- GlobalOnly. Switch to the branch `GlobalOnly`.


# Testing the Experiment

Things to test:
- Players dropout - are they handled correctly?
- Group of 4 players arrive - are they grouped up? 
- Grouping is done correctly (at the game stage). 
- Group IDs match the groups.
- id_in_group matches the local groups.
- Run through game ok.
- Run through the entire experiments ok. 
- BEFORE deploying: Are timeouts the correct legnth of time? 