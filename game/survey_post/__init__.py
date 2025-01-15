import time
import random

from otree.api import *

from game import C as gameConstants

doc = """
Your app description
"""

class C(BaseConstants):
    NAME_IN_URL = 'survey_post'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1

class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass
def make_field(label):
    return models.IntegerField(
        choices=[1,2,3,4,5],
        label=label,
        widget=widgets.RadioSelect,
    )


class Player(BasePlayer):


    survey_global_sis_1 = make_field("What each of us does in this situation affects the other.")
    survey_global_sis_2 = make_field("We can both obtain our preferred outcomes.")
    survey_global_sis_3 = make_field("Whatever each of us does in this situation, our actions will not affect the other’s outcomes")
    survey_global_sis_4 = make_field("Our preferred outcomes in this situation are conflicting.")

    survey_local_sis_1 = make_field("What each of us does in this situation affects the other.")
    survey_local_sis_2 = make_field("We can both obtain our preferred outcomes.")
    survey_local_sis_3 = make_field("Whatever each of us does in this situation, our actions will not affect the other’s outcomes")
    survey_local_sis_4 = make_field("Our preferred outcomes in this situation are conflicting.")

    survey_other_sis_1 = make_field("What each of us does in this situation affects the other.")
    survey_other_sis_2 = make_field("We can both obtain our preferred outcomes.")
    survey_other_sis_3 = make_field("Whatever each of us does in this situation, our actions will not affect the other’s outcomes")
    survey_other_sis_4 = make_field("Our preferred outcomes in this situation are conflicting.")    

    attention_check_1 = make_field("This is not a real question, we are just checking if you are reading the questions. Please select 'Slightly Agree'")       

    survey_strategy_1 = make_field("I wanted to make Allshire meet the target.")
    survey_strategy_2 = make_field("I wanted to make Westville meet the target.")
    survey_strategy_3 = make_field("I wanted to make Eastburgh meet the target.")
    survey_strategy_4 = make_field("I wanted to maximise my own winnings.")
    survey_strategy_5 = make_field("I wanted the entire group to do well.")
    survey_strategy_6 = make_field("I wanted to get more coins that anyone else.")
    survey_strategy_7 = make_field("I was just clicking randomly.")

    survey_strategy_free_text = models.LongStringField(
        label="Please tell us anything else about the game and your experience. For example this could be about your strategy, about the other players or about how you felt during the game."
        )


class SurveySIS(Page):
    form_model = 'player'
    form_fields = [
        'survey_global_sis_1',
        'survey_global_sis_2',
        'survey_global_sis_3',
        'survey_global_sis_4',
        'attention_check_1',
        'survey_local_sis_1',
        'survey_local_sis_2',
        'survey_local_sis_3',
        'survey_local_sis_4',
        'survey_other_sis_1',
        'survey_other_sis_2',
        'survey_other_sis_3',
        'survey_other_sis_4',
        ]
   

class SurveyStrategy(Page):

    form_model = 'player'
    form_fields = [
        'survey_strategy_1',
        'survey_strategy_2',
        'survey_strategy_3',
        'survey_strategy_4',
        'survey_strategy_5',
        'survey_strategy_6',
        'survey_strategy_7',
        'survey_strategy_free_text'
        ]



page_sequence = [
    SurveySIS, SurveyStrategy
]
