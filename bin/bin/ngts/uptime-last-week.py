#!/usr/local/python/bin/python
# -*- coding: utf-8 -*-

import pymysql
from pymysql.cursors import DictCursor
import datetime

now = datetime.datetime.utcnow()
last_week = (now - datetime.timedelta(days=7)).date()

with pymysql.connect(user='sw', host='ngtsdb',
                     db='ngts_ops', cursorclass=DictCursor) as cursor:
    cursor.execute('''select * from action_summary_log
                   where night >= %s
                   and status = "completed"
                   order by night asc, camera_id asc''', (last_week, ))
    rows = cursor.fetchall()

valid_camera_ids = set(range(801, 814)) - set([807])
distinct_nights = set([row['night'] for row in rows])
distinct_camera_ids = set([row['camera_id'] for row in rows
                          if row['camera_id'] in valid_camera_ids])
print('Observed {} nights'.format(len(distinct_nights)))

for night in sorted(list(distinct_nights)):
    print('{}'.format(night))
    actions_in_night = [row for row in rows if row['night'] == night]

    for camera_id in sorted(distinct_camera_ids):
        print('\t{}'.format(camera_id))
        actions_for_this_camera = [row for row in actions_in_night
                                   if row['camera_id'] == camera_id]
        fields = ', '.join(['{} ({})'.format(row['field'], row['num_images'])
                            for row in actions_for_this_camera
                           if row['action_type'] == 'observeField'])
        print('\t\t{}'.format(fields))

        nbiases = sum(row['num_images'] for row in actions_for_this_camera
                     if row['action_type'] == 'biasFrames')
        ndarks = sum(row['num_images'] for row in actions_for_this_camera
                     if row['action_type'] == 'darkFrames')
        nflats = sum(row['num_images'] for row in actions_for_this_camera
                     if row['action_type'] == 'flatField')
        print('\t\t{} biases'.format(nbiases))
        print('\t\t{} darks'.format(ndarks))
        print('\t\t{} flats'.format(nflats))



