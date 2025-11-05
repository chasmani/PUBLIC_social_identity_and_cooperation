import time
import random

from otree.api import *

from game import C as gameConstants

doc = """
Your app description
"""

class C(BaseConstants):
    NAME_IN_URL = 'instructions'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1

class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):

    quiz_1_how_many_rounds = models.StringField(
        label="How many rounds will you play?",
        choices=["Up to 20", "Up to 25", "Up to 30"],
        widget=widgets.RadioSelect)

    quiz_2_players_allshire = models.IntegerField(
        label="How many players in total are in Allshire?")

    quiz_2_players_westville = models.IntegerField(
        label="How many players in total are in Westville?")

    quiz_2_players_eastburgh = models.IntegerField(
        label="How many players in total are in Eastburgh?")

    quiz_2_where_are_you = models.StringField(
        label="Which areas are you a part of",
        choices=["Westville and Eastburgh", "Westville and Allshire", "Only Westville", "Only Allshire"],
        widget=widgets.RadioSelect)


    quiz_3_which_choices = models.StringField(
        label = "Which choices do you have each round?",
        choices=[
            "a) Invest in Westville b) Keep the coin",
            "a) Invest in Allshire b) Invest in Westville c) keep the coin",
            "a) Invest in Allshire b) invest in Westville c) Invest in Eastburgh d) Keep the coin"],
            widget=widgets.RadioSelect
        )

    quiz_3_allshire_threshold = models.IntegerField(
        label = "At least how many players need to choose Allshire to earn the Allshire bonus?",
        )

    quiz_3_westville_threshold = models.IntegerField(
        label = "At least how many players need to choose Westville to earn the Westville bonus?",
        )    

    quiz_3_westville_bonus_who = models.StringField(
        label = "If 2 players invest in Westville, who gets the Westville bonus?",
        choices=[
            "Nobody",
            "The 2 investors",
            "All 3 Westville players",
            "All 6 players in Allshire"],
        widget=widgets.RadioSelect
        )

    quiz_3_all_players_allshire_bonus = models.BooleanField(
        label = "Do all players get the Allshire bonus if the Allshire target is met.",
        widget=widgets.RadioSelect
        )

    quiz_3_keep_coin = models.BooleanField(
        label = "If you choose to keep the coin, can you still earn other bonuses if the other players meet the targets.",
        widget=widgets.RadioSelect
        )

    quiz_3_eastburgh_bonus = models.BooleanField(
        label = "As a Westville player. Do you earn any bonus if the Eastburgh target is met? ",
        widget=widgets.RadioSelect
        )

    scenario_1_allshire_meet_target = models.BooleanField(
        label = "Did the group meet the Allshire target?",
        widget = widgets.RadioSelect
        )

    scenario_1_westville_meet_target = models.BooleanField(
        label = "Did Westville meet the target?",
        widget = widgets.RadioSelect
        )

    scenario_1_how_many_coins = models.FloatField(
        label = "How many coins will you earn this round?"
        )

    scenario_2_allshire_meet_target = models.BooleanField(
        label = "Did the group meet the Allshire target?",
        widget = widgets.RadioSelect
        )

    scenario_2_counterfactual_allshire_meet_target = models.BooleanField(
        label = "If you had kept the coin, and everyone else played the same way, would the group still have met the Allshire target?",
        widget = widgets.RadioSelect
        )

    scenario_2_counterfactual_payoff = models.FloatField(
        label = "If you had kept the coin, and everyone else played the same way, how much would you personally have earned this round?"
        )    



class Instructions1(Page):

    form_model = 'player'
    form_fields = [
        'quiz_1_how_many_rounds',
        ]

    @staticmethod
    def error_message(player, values):
        if values['quiz_1_how_many_rounds'] != "Up to 25":
            return 'That is incorrect. Please try again.'

    @staticmethod
    def before_next_page(player, timeout_happened):

        player.participant.wrong_answers = []

class Instructions2(Page):

    form_model = 'player'
    form_fields = [
        'quiz_2_players_allshire',
        'quiz_2_players_westville',
        'quiz_2_players_eastburgh',
        'quiz_2_where_are_you'
        ]

    @staticmethod
    def error_message(player, values):

        error_message_string = "One or more answers are incorrect. Please try again."
        errors = []

        if values['quiz_2_players_allshire'] != 6:
            errors.append('quiz_2_players_allshire')
        if values['quiz_2_players_westville'] != 3:
            errors.append('quiz_2_players_westville')
        if values['quiz_2_players_eastburgh'] != 3:
            errors.append('quiz_2_players_eastburgh')
        if values['quiz_2_where_are_you'] != "Westville and Allshire":
            errors.append('quiz_2_where_are_you')

        if len(errors) > 0:
            player.participant.wrong_answers.append(errors)
            return error_message_string


class Instructions3(Page):

    form_model = 'player'
    form_fields = [
        'quiz_3_allshire_threshold',
        'quiz_3_westville_threshold',
        'quiz_3_westville_bonus_who',
        'quiz_3_all_players_allshire_bonus',
        'quiz_3_keep_coin',
        'quiz_3_eastburgh_bonus'
        ]

    @staticmethod
    def error_message(player, values):
        error_message_string = "One or more answers are incorrect. Please try again."
        errors = []
        if values['quiz_3_allshire_threshold'] != 4:
            errors.append('quiz_3_allshire_threshold')
        if values['quiz_3_westville_threshold'] != 2:
            errors.append('quiz_3_westville_threshold')
        if values['quiz_3_westville_bonus_who'] != "All 3 Westville players":
            errors.append('quiz_3_westville_bonus_who')
        if values['quiz_3_all_players_allshire_bonus'] != True:
            errors.append('quiz_3_all_players_allshire_bonus')
        if values['quiz_3_keep_coin'] != True:
            errors.append('quiz_3_keep_coin')

        if len(errors) > 0:
            player.participant.wrong_answers.append(errors)
            return error_message_string
        
    @staticmethod
    def vars_for_template(player):

        return {
            "gameConstants":gameConstants      
            }

class Scenario1(Page):

    form_model = 'player'
    form_fields = [
        'scenario_1_allshire_meet_target',
        'scenario_1_westville_meet_target',
        'scenario_1_how_many_coins',
        ]

    @staticmethod
    def error_message(player, values):
        error_message_string = "One or more answers are incorrect. Please try again."
        errors = []
        if values['scenario_1_allshire_meet_target'] != False:
            errors.append('scenario_1_allshire_meet_target')
        if values['scenario_1_westville_meet_target'] != True:
            errors.append('scenario_1_westville_meet_target')
        if values['scenario_1_how_many_coins'] != gameConstants.LOCAL_REWARD + 1:
            errors.append('scenario_1_how_many_coins')

        if len(errors) > 0:
            player.participant.wrong_answers.append(errors)
            return error_message_string
        
    @staticmethod
    def vars_for_template(player):

        return {
            "gameConstants":gameConstants      
            }

class Scenario2(Page):

    form_model = 'player'
    form_fields = [
        'scenario_2_allshire_meet_target',
        'scenario_2_counterfactual_allshire_meet_target',
        'scenario_2_counterfactual_payoff',
        ]

    @staticmethod
    def error_message(player, values):
        error_message_string = "One or more answers are incorrect. Please try again."
        errors = []
        if values['scenario_2_allshire_meet_target'] != True:
            errors.append('scenario_2_allshire_meet_target')
        if values['scenario_2_counterfactual_allshire_meet_target'] != True:
            errors.append('scenario_2_counterfactual_allshire_meet_target')
        if values['scenario_2_counterfactual_payoff'] != gameConstants.GLOBAL_REWARD + 1:
            errors.append('scenario_2_counterfactual_payoff')

        if len(errors) > 0:
            player.participant.wrong_answers.append(errors)
            return error_message_string
        
    @staticmethod
    def vars_for_template(player):

        return {
            "gameConstants":gameConstants      
            }


page_sequence = [
    Instructions1, Instructions2, Instructions3, Scenario1, Scenario2
]
