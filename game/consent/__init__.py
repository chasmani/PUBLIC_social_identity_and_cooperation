from otree.api import *

import time

doc = """
Your app description
"""


class C(BaseConstants):
    NAME_IN_URL = 'consent'
    PLAYERS_PER_GROUP = None
    NUM_ROUNDS = 1


class Subsession(BaseSubsession):
    pass


class Group(BaseGroup):
    pass


class Player(BasePlayer):
    consent_1 = models.BooleanField(label="I have read the information above and understand I can email the researchers with any questions.",
                                  choices=[
                                      [True, "Yes"],
                                  ]
                                  )

    consent_2 = models.BooleanField(label="I understand that my personal information will be used for the purposes explained to me. I understand that according to data protection legislation, 'public task' will be the lawful basis for processing.",
                                  choices=[
                                      [True, "Yes"],
                                  ]
                                  )


    consent_3 = models.BooleanField(label="I understand that all personal information will remain confidential and that my data gathered in this study will be stored anonymously and securely. It will not be possible to identify me in any publications.",
                                  choices=[
                                      [True, "Yes"],
                                  ]
                                  )


    consent_4 = models.BooleanField(label="I understand that my anonymised research data may be shared with, and used by, others for future research (no one will be able to identify you when these data are shared).",
                                  choices=[
                                      [True, "Yes"],
                                  ]
                                  )


    consent_5 = models.BooleanField(label="I understand that I am free to withdraw from the study without penalty if I so wish, simply by closing my browser window.",
                                  choices=[
                                      [True, "Yes"],
                                  ]
                                  )

    consent_6 = models.BooleanField(label="I consent to take part in the study.",
                                  choices=[
                                      [True, "Yes"],
                                  ]
                                  )


class ParticipantConsent(Page):
    form_model = "player"
    form_fields = ["consent_1", "consent_2", "consent_3", "consent_4", "consent_5", "consent_6"]

    @staticmethod
    def before_next_page(player, timeout_happened):
        # Keep the group ids given at the start
        # To avoid group correlations

        player.participant.past_group_id = player.group.id
        player.participant.is_dropout = False
        player.participant.timeouts = []


class Demographics(Page):
	pass

page_sequence = [ParticipantConsent] 