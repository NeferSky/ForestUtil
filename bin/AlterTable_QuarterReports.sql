alter table QuarterReports
add report_quarter integer,
add report_year integer;

update QuarterReports
set report_quarter = 3,
report_year = 2016;