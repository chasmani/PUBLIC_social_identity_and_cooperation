import time
import random

from otree.api import *

from game import C as gameConstants

doc = """
Your app description
"""

class C(BaseConstants):
    NAME_IN_URL = 'survey'
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

    """
    #survey_group_1 = make_field("When a player from Allshire succeeds, I feel good.")
    #survey_group_2 = make_field("When a player from Allshire fails, I feel bad.")
    survey_group_3 = make_field("I feel that a player from Allshire's gain is my gain.")
    survey_group_4 = make_field("What is good for a player from Allshire is good for me.")
    survey_group_5 = make_field("Honestly, I don't care whether a player from Allshire thrives of not.")
    survey_group_6 = make_field("A player from Allshire and I rise and fall together.")
    survey_group_9 = make_field("Whatever each of us does in this situation, our actions will not affect the other’s outcomes")
    survey_group_10 = make_field("Our preferred outcomes in this situation are conflicting.")
    survey_neighbourhood_1 = make_field("When a player from Westville succeeds, I feel good.")
    survey_neighbourhood_2 = make_field("When a player from Westville fails, I feel bad.")
    survey_neighbourhood_3 = make_field("I feel that a player from Westville's gain is my gain.")
    survey_neighbourhood_4 = make_field("What is good for a player from Westville is good for me.")
    survey_neighbourhood_5 = make_field("Honestly, I don't care whether a player from Westville thrives of not.")
    survey_neighbourhood_6 = make_field("A player from Westville and I rise and fall together.")
    survey_neighbourhood_7 = make_field("What each of us does in this situation affects the other.")
    survey_neighbourhood_8 = make_field("We can both obtain our preferred outcomes.")
    survey_neighbourhood_9 = make_field("Whatever each of us does in this situation, our actions will not affect the other’s outcomes")
    survey_neighbourhood_10 = make_field("Our preferred outcomes in this situation are conflicting.")
    """


    survey_global_sis_1 = make_field("What each of us does in this situation affects the other.") # MD 
    survey_global_sis_2 = make_field("We can both obtain our preferred outcomes.") # COnflict
    survey_global_sis_3 = make_field("Whatever each of us does in this situation, our actions will not affect the other’s outcomes") # MD
    survey_global_sis_4 = make_field("Our preferred outcomes in this situation are conflicting.") # Conflict

    survey_local_sis_1 = make_field("What each of us does in this situation affects the other.")
    survey_local_sis_2 = make_field("We can both obtain our preferred outcomes.")
    survey_local_sis_3 = make_field("Whatever each of us does in this situation, our actions will not affect the other’s outcomes")
    survey_local_sis_4 = make_field("Our preferred outcomes in this situation are conflicting.")    

    survey_other_sis_1 = make_field("What each of us does in this situation affects the other.")
    survey_other_sis_2 = make_field("We can both obtain our preferred outcomes.")
    survey_other_sis_3 = make_field("Whatever each of us does in this situation, our actions will not affect the other’s outcomes")
    survey_other_sis_4 = make_field("Our preferred outcomes in this situation are conflicting.") 

    attention_check_1 = make_field("This is not a real question, we are just checking if you are reading the questions. Please select 'Slightly Disagree'")   

    expected_global = models.IntegerField(min=0, max=100, label="How many would choose Allshire? (Out of 100 people)")
    expected_local = models.IntegerField(min=0, max=100, label="How many would choose Westville? (Out of 100 people)")
    expected_other = models.IntegerField(min=0, max=100, label="How many would choose Eastburgh? (Out of 100 people)")
    expected_defect = models.IntegerField(min=0, max=100, label="How many would choose to keep the coin? (Out of 100 people)")

    DIFI_big_group_distance = models.FloatField(min=-200)
    DIFI_my_group_distance = models.FloatField(min=-200)
    DIFI_other_group_distance = models.FloatField(min=-200)
    

class Instructions(Page):

    @staticmethod
    def vars_for_template(player: Player):
        return {"gameConstants": gameConstants}

    
class GetReady(Page):

     # If we're out of time move to next app in the sequence
    @staticmethod
    def before_next_page(player, timeout_happened):
        player.participant.wait_page_arrival = time.time()
        

class GroupIdentity(Page):

    form_model = "player"
    form_fields = ["DIFI_big_group_distance", "DIFI_my_group_distance", "DIFI_other_group_distance"]

    @staticmethod
    def before_next_page(player, timeout_happened):
        player.participant.big_group_position = player.DIFI_big_group_distance
        player.participant.my_group_position = player.DIFI_my_group_distance
        player.participant.other_group_position = player.DIFI_other_group_distance
        print(player.participant.vars)

class ExpectedActions(Page):

    form_model = 'player'
    form_fields = ['expected_global', 'expected_local', 'expected_other', 'expected_defect']

    @staticmethod
    def error_message(player, values):
        print('values is', values)
        if values['expected_global'] + values['expected_local'] + values['expected_other'] + values['expected_defect'] != 100:
            return 'The numbers must add up to 100'


class SurveySIS(Page):
    form_model = 'player'
    form_fields = [
        'survey_global_sis_1',
        'survey_global_sis_2',
        'survey_global_sis_3',
        'survey_global_sis_4',
        'survey_local_sis_1',
        'survey_local_sis_2',
        'survey_local_sis_3',
        'survey_local_sis_4',
        'attention_check_1',
        'survey_other_sis_1',
        'survey_other_sis_2',
        'survey_other_sis_3',
        'survey_other_sis_4',
        ]


page_sequence = [
    SurveySIS, GroupIdentity, ExpectedActions, GetReady
]