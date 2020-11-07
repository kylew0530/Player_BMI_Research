use baseball;

#Getting Hitter Data#
select batter_id, player_name, 
ROUND(avg(launch_speed),1) as avg_ev, max(launch_speed) as max_ev, min(launch_speed) as min_ev, ROUND(stddev(launch_speed),2) as std_ev,  
ROUND(avg(launch_angle),1) as avg_la, max(launch_angle) as max_la, min(launch_angle) as min_la, ROUND(stddev(launch_angle),2) as std_la,
COUNT(*) AS number_of_batted_balls
from hit_by_row
group by batter_id, player_name
order by player_name;

#Getting Pitcher Data#
select pitcher_id, pitcher_name as player_name, 
ROUND(avg(release_speed),1) as avg_velocity, max(release_speed) as max_velocity, min(release_speed) as min_velocity, ROUND(stddev(release_speed),2) as velocity_std,
COUNT(*) AS number_of_pitches
from pitch_by_row
where pitch_name in ('4-Seam Fastball', 'Sinker', '2-Seam Fastball') and pitcher_name!='NA'
group by pitcher_id, player_name
order by player_name;
