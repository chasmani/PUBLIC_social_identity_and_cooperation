from os import environ


SESSION_CONFIGS = [
    dict(
        name='instructions',
        display_name="Instructions",
        app_sequence=['instructions'],
        num_demo_participants=12,
    ),
    dict(
        name='game',
        display_name="Game",
        app_sequence=['game'],
        num_demo_participants=14,
    ),
    dict(
        name='pre_game_survey',
        display_name="Pre Game Survey",
        app_sequence=['survey'],
        num_demo_participants=12,
    ),
    dict(
        name='post_game_survey',
        display_name="Post Game Survey",
        app_sequence=['survey_post'],
        num_demo_participants=12,
    ),

    dict(
        name='full_experiment',
        display_name="Full Experiment",
        app_sequence=['consent', 'instructions', 'survey', 'game', 'survey_post', 'payment'],
        num_demo_participants=12,
    ),
    dict(
        name='survey_and_game',
        display_name="Survey and Game Only",
        app_sequence=['survey', 'game'],
        num_demo_participants=12,
    ),
]

# if you set a property in SESSION_CONFIG_DEFAULTS, it will be inherited by all configs
# in SESSION_CONFIGS, except those that explicitly override it.
# the session config can be accessed from methods in your apps as self.session.config,
# e.g. self.session.config['participation_fee']

SESSION_CONFIG_DEFAULTS = dict(
    real_world_currency_per_point=1.00, 
    participation_fee=0.00, 
    doc="",
)

# These fields used to store data for participants between apps and rounds
PARTICIPANT_FIELDS = [
    "total_payoff", 
    "short_survey_global",
    "short_survey_local",
    "global_group_history",
    "local_group_history",
    "other_group_history",
    "kept_group_history",
    "big_group_position",
    "my_group_position",
    "other_group_position",
    "wrong_answers",
    "past_group_id",
    "unique_group_id",
    "custom_id_in_group",
    "wait_page_arrival",
    "app_stage",
    "is_dropout",
    "timeouts",
    ]

# WARNING - SESSION CONFIGS ARE SHARED AMONG ALL PLAYERS IN A SESSION, ACROSS GROUPS
SESSION_FIELDS = [
    ]

# ISO-639 code
# for example: de, fr, ja, ko, zh-hans
LANGUAGE_CODE = 'en'

# e.g. EUR, GBP, CNY, JPY
REAL_WORLD_CURRENCY_CODE = 'USD'
USE_POINTS = True

ROOMS = [
    dict(
        name="full_experiment",
        display_name = "Full Experiment",
      #  participant_label_file='_rooms/participant_labels.txt',
    )
]

ADMIN_USERNAME = 'admin'
# for security, best to set admin password in an environment variable
ADMIN_PASSWORD = environ.get('OTREE_ADMIN_PASSWORD')

DEMO_PAGE_INTRO_HTML = """
Here are some oTree games.
"""

SECRET_KEY = '5134823306042'

INSTALLED_APPS = ['otree']
