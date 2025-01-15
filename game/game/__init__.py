import random
import itertools  
import time

import numpy as np

from collections import Counter
import itertools

from otree.api import *

doc = """
Your app description
"""

def creating_session(subsession):

    if subsession.round_number == 1:
        # The below code only runs if we are running this app in isolation - not in the full experiment
        # If running this app in isolation, set the participant vars so that it works
        # For the full experiment, wait_page_arrival is set in previous app
        # For testing


        if subsession.session.config["name"] == "game":
            
            players = subsession.get_players()

            player_counter = 0
            group_id =1 
            for player in players:

                player.participant.wait_page_arrival = time.time()

                player.participant.is_dropout = False
                player.participant.timeouts = []

                player.participant.big_group_position = -25
                player.participant.my_group_position = -25
                player.participant.other_group_position = -25

                player.participant.wrong_answers = []


class C(BaseConstants):
    NAME_IN_URL = 'game'
    PLAYERS_PER_GROUP = None
    PLAYERS_PER_LOCAL_GROUP = 3
    NUM_ROUNDS = 20
    LOCAL_THRESHOLD = 2
    GLOBAL_THRESHOLD = 4
    LOCAL_REWARD = 2
    GLOBAL_REWARD = 2.5
    ENDOWMENT = 1
    TIME_OUT = 30
    GROUPING_TIME_OUT = 480
    PAYOUT_PER_COIN = 0.05
    SIGNAL = False
    SIGNAL_ROUND = 10
    SIGNAL_GLOBAL_REWARD = 10


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    contribution = models.StringField(
        label="You are given 1 coin, what would you like to do?",
        choices=[
            ['global', 'Invest in Allshire'],
            ['local', 'Invest in Westville'],
            ['other', 'Invest in Eastburgh'],
            ['self', 'Keep the coin'],
        ],
        widget=widgets.RadioSelect,
        initial=''
    )
    local_threshold_met = models.BooleanField(default=False)
    global_threshold_met = models.BooleanField(default=False)
    other_threshold_met = models.BooleanField(default=False)
    local_group_contributions = models.IntegerField(default=0)
    global_group_contributions = models.IntegerField(default=0)
    kept_group_contributions = models.IntegerField(default=0)
    other_group_contributions = models.IntegerField(default=0)
    total_payoff_this_round = models.FloatField(default=0)

    DIFI_big_group_distance = models.FloatField(min=-200)
    DIFI_my_group_distance = models.FloatField(min=-200)
    DIFI_other_group_distance = models.FloatField(min=-200)

    signal_up = models.IntegerField(
        label="What is the Allshire bonus this round?"
    )
    signal_down = models.IntegerField(
        label="What is the Allshire bonus this round?"
    )



def waiting_too_long(player):
    print("Are we waiting too long?")
    return time.time() - player.participant.wait_page_arrival > C.GROUPING_TIME_OUT


def group_by_arrival_time_method(self, waiting_players):

    print(waiting_players)

    if len(waiting_players) >= 6:
        this_group = waiting_players[:6]
        unique_group_id = np.random.randint(1000000000)
        ids_in_group = itertools.cycle([1,2,3,4,5,6])
        for player in this_group:
            player.participant.app_stage = "Game"
            player.participant.unique_group_id = unique_group_id
            player.participant.custom_id_in_group = next(ids_in_group)

        return this_group
    
    print("I'm here")
    for player in waiting_players:
        print("Gonna check")
        if waiting_too_long(player):
            if len(waiting_players) >= 6:
                this_group = waiting_players[:6]
            else:
                this_group = waiting_players
            unique_group_id = np.random.randint(1000000000)
            ids_in_group = itertools.cycle([1,2,3,4,5,6])
            for player in this_group:
                player.participant.app_stage = "Game"
                player.participant.unique_group_id = unique_group_id
                player.participant.custom_id_in_group = next(ids_in_group)
            return this_group

class GroupWaitPage(WaitPage):

    group_by_arrival_time = True

    body_text = """
        <p>IT IS IMPORTANT THAT YOU KEEP THIS TAB ACTIVE.</p>
        You may have to wait a few minutes while the other players in your group catch up.
        Once everyone catches up the game will start immediately. 
        """

    @staticmethod
    def is_displayed(player):
        return player.round_number == 1


class Contribution(Page):

    form_model = 'player'
    form_fields = ['contribution']

    @staticmethod
    def vars_for_template(player):

        if player.round_number > 1:

            chart_max_round = player.round_number -1
            if chart_max_round < 25:
                chart_max_round = 25

            round_nums = list(range(1, chart_max_round + 1))

            global_thresholds = [C.GLOBAL_THRESHOLD] * (chart_max_round)
            local_thresholds = [C.LOCAL_THRESHOLD] * (chart_max_round)

            return {
                "round_nums":round_nums,
                "global_thresholds":global_thresholds,
                "local_thresholds":local_thresholds, 
                "global_history": player.participant.global_group_history,
                "local_history": player.participant.local_group_history,
                "other_history": player.participant.other_group_history,
                "kept_history": player.participant.kept_group_history,      
            }

    @staticmethod
    def is_displayed(player):
        return not player.participant.is_dropout

    @staticmethod
    def get_timeout_seconds(player):

        if player.participant.is_dropout:
            return 1  # instant timeout, 1 second
        else:
            return C.TIME_OUT


    @staticmethod
    def before_next_page(player, timeout_happened):

        if timeout_happened:

            timeout_string =  "Contribution_round_{}".format(player.round_number)
            player.participant.timeouts.append(timeout_string)

            print(player.participant.timeouts)

            if len(player.participant.timeouts) >= 3:
                player.participant.is_dropout = True


def get_results(group):

    if group.round_number == 1:
        for player in group.get_players():
            player.participant.total_payoff = 0
            player.participant.global_group_history = []
            player.participant.local_group_history = []
            player.participant.other_group_history = []
            player.participant.kept_group_history = []


    # 1. Get a list of 6 actions. Either real or simulated for each player
    players = group.get_players()

    # Sort plaeyrs to make sure they are in order of id_in_group 
    players = sorted(players, key=lambda x: x.participant.custom_id_in_group)

    # Actions align with players. Same indices
    actions = [player.field_maybe_none('contribution') for player in players]
    # E.G. ['global', 'local', 'global', 'global', 'global', 'self']
    # E.G. ['global', 'self']
    # E.g. ['', None, 'self', None, None, None]
    # Make the actions 6 long. 
    while len(actions) < 6:
        actions.append('')

    # Simulate actions for any players who have dropped out/missing actions. 
    # Get group totals so far. Add one to all of them in case they are zero
    total_global = sum(players[0].participant.global_group_history) + 1 
    total_local = sum(players[0].participant.local_group_history) + sum(players[0].participant.other_group_history) + 1
    total_kept = sum(players[0].participant.kept_group_history) + 1
    total_all = total_global + total_local + total_kept
    actions_probs = [total_global/total_all, total_local/total_all, total_kept/total_all]
    all_actions = []
    for action in actions:
        if action == "":
            action = np.random.choice(["global", "local", "self"], p=actions_probs)
        all_actions.append(action)

    print("ACTIONS ARE ", all_actions)

    for i in range(6):
        try:
            player = players[i]

            global_count = all_actions.count("global")
            kept_count = all_actions.count('self')

            if i < 3:
                local_count = all_actions[:3].count("local") + all_actions[3:].count("other")
                print(all_actions, local_count)
                other_count = all_actions[3:].count("local") + all_actions[:3].count("other")
            else:
                local_count = all_actions[3:].count("local") + all_actions[:3].count("other")
                other_count = all_actions[:3].count("local") + all_actions[3:].count("other")

            player.local_group_contributions = local_count
            if local_count >= C.LOCAL_THRESHOLD:
                player.local_threshold_met = True
                player.total_payoff_this_round += C.LOCAL_REWARD

            player.global_group_contributions = global_count
            if global_count >= C.GLOBAL_THRESHOLD:
                player.global_threshold_met = True
                if C.SIGNAL and player.round_number == C.SIGNAL_ROUND:
                    player.total_payoff_this_round += C.SIGNAL_GLOBAL_REWARD
                else:
                    player.total_payoff_this_round += C.GLOBAL_REWARD

            player.other_group_contributions = other_count
            if other_count >= C.LOCAL_THRESHOLD:
                player.other_threshold_met = True

            player.kept_group_contributions = kept_count

            if player.contribution == "self":
                player.total_payoff_this_round += 1

            player.participant.total_payoff += player.total_payoff_this_round

            player.participant.global_group_history.append(global_count)
            player.participant.local_group_history.append(local_count)
            player.participant.other_group_history.append(other_count)
            player.participant.kept_group_history.append(kept_count)
        # If not 6 players, skip and players who are not in the group
        except IndexError:
            pass

        
def generate_group_result_classes(n_invested, threshold, n_players):

    result_classes = ["empty"] * n_players
    for i in range(n_invested):
        result_classes[i] = "full"
    result_classes = result_classes[:threshold] + ["threshold"] + result_classes[threshold:]
    return result_classes

class ResultWaitPage(WaitPage):   

    after_all_players_arrive = 'get_results'

    @staticmethod
    def is_displayed(player):
        return not player.participant.is_dropout

class Result(Page):

    @staticmethod
    def is_displayed(player):
        return not player.participant.is_dropout

    @staticmethod
    def vars_for_template(player):

        global_results_classes = generate_group_result_classes(player.global_group_contributions, C.GLOBAL_THRESHOLD, 6)
        local_results_classes = generate_group_result_classes(player.local_group_contributions, C.LOCAL_THRESHOLD, 6)
        other_results_classes = generate_group_result_classes(player.other_group_contributions, C.LOCAL_THRESHOLD, 6)

        global_reward = C.GLOBAL_REWARD
        local_reward = C.LOCAL_REWARD
        if C.SIGNAL and player.round_number == C.SIGNAL_ROUND:
            global_reward = C.SIGNAL_GLOBAL_REWARD

        return {
            "global_reward":global_reward,
            "local_reward":local_reward,
            "global_results_classes":global_results_classes,
            "local_results_classes":local_results_classes,
            "other_results_classes":other_results_classes,         
        }

    @staticmethod
    def get_timeout_seconds(player):
        return C.TIME_OUT


def get_style_from_position(position):
    """
    Convert position on the Dynamics ID Fusion scale into a style
    Style of left goes from 0 to 75
    Scale goes from -175 to 125
    """
    style = (position + 175)/300 * 75   
    style_string = "{}%".format(style)
    return style_string
    
class GroupIdentity(Page):
    
    form_model = "player"
    form_fields = ["DIFI_big_group_distance", "DIFI_my_group_distance", "DIFI_other_group_distance"]

    @staticmethod
    def before_next_page(player, timeout_happened):
        if not timeout_happened:
            player.participant.big_group_position = player.DIFI_big_group_distance
            player.participant.my_group_position = player.DIFI_my_group_distance
            player.participant.other_group_position = player.DIFI_other_group_distance

    @staticmethod
    def is_displayed(player):
        if player.participant.is_dropout:
            return False
        return player.round_number % 5 == 0

    @staticmethod
    def vars_for_template(player):

        print(player.participant.big_group_position)

        return {
            "big_group_position":get_style_from_position(player.participant.big_group_position),
            "my_group_position":get_style_from_position(player.participant.my_group_position),
            "other_group_position":get_style_from_position(player.participant.other_group_position)
            }

    @staticmethod
    def get_timeout_seconds(player):
        return C.TIME_OUT * 2


class SignalUp(Page):

    form_model = "player"
    form_fields = ["signal_up"]

    @staticmethod
    def is_displayed(player):
        if player.participant.is_dropout:
            return False
        if C.SIGNAL:
            return player.round_number == C.SIGNAL_ROUND
        else:
            return False
        
    @staticmethod
    def error_message(player, values):
        error_message_string = "That is incorrect. Please try again."
        errors = []
        if values['signal_up'] != C.SIGNAL_GLOBAL_REWARD:
            errors.append('signal_up')
            player.participant.wrong_answers.append(errors)
            return error_message_string

    @staticmethod
    def get_timeout_seconds(player):
        return C.TIME_OUT * 2

class SignalDown(Page):

    form_model = "player"
    form_fields = ["signal_down"]

    @staticmethod
    def is_displayed(player):
        if player.participant.is_dropout:
            return False
        if C.SIGNAL:
            return player.round_number == C.SIGNAL_ROUND + 1
        else:   
            return False
        
    @staticmethod
    def error_message(player, values):
        error_message_string = "That is incorrect. Please try again."
        errors = []
        if values['signal_down'] != C.GLOBAL_REWARD:
            errors.append('signal_down')
            player.participant.wrong_answers.append(errors)
            return error_message_string
        
    @staticmethod
    def get_timeout_seconds(player):
        return C.TIME_OUT * 2


page_sequence = [
    GroupWaitPage,
    SignalUp,
    SignalDown,
    Contribution, 
    ResultWaitPage, 
    Result,
    GroupIdentity
    ]