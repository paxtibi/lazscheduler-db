# lazscheduler-db
lazscheduler database
## dependences
Zeosdb 

## Configuration lazScheduler.conf example mariadb/mysql
```
dbc.url=zdbc:MariaDB-5://localhost:3306/<database>?username=<username>;password=<password>
logger.trigger=Info
parallel=1
execute-after-connection=SET NAMES utf8;SET sql_mode=(SELECT REPLACE(@@sql_mode,'ONLY_FULL_GROUP_BY',''))
table-prefix=oc_
# maxloops -1 = forever, set e.g. to 2 to get stop after 2 iterations
maxloops=-1
# seconds to wait between loops
wait_seconds_after=30
```

## tables on database example mariadb/mysql

```sql
CREATE TABLE `<table-prefix>scheduler` (
  `id` BIGINT(20) NOT NULL AUTO_INCREMENT,
  `command` VARCHAR(1000) NOT NULL COLLATE 'utf8_general_ci',
  `running` INT(1) NOT NULL DEFAULT '0',
  `done` INT(1) NOT NULL DEFAULT '0',
  `scheduled_at` DATETIME NOT NULL,
  `parameters` VARCHAR(1000) NULL DEFAULT NULL COLLATE 'utf8_general_ci',
  `RETURN_CODE` INT(11) NULL DEFAULT NULL,
  `created` DATETIME NULL DEFAULT CURRENT_TIMESTAMP,
  `update` DATETIME NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `resched_timescale` VarChar( 1 ) CHARACTER SET utf8 COLLATE utf8_general_ci NULL DEFAULT NULL,
  `resched_value` Int( 11 ) NULL DEFAULT NULL,
  PRIMARY KEY (`id`) USING BTREE
)
COLLATE='utf8_general_ci'
ENGINE=MyISAM;

CREATE TABLE `<table-prefix>scheduler_job` (
  `id` BIGINT(20) NOT NULL AUTO_INCREMENT,
  `scheduler_id` BIGINT(20) NOT NULL,
  `pid` INT(11) NOT NULL,
  PRIMARY KEY (`id`) USING BTREE
)
COLLATE='utf8_general_ci'
ENGINE=MyISAM;


CREATE TABLE `<table-prefix>scheduler_job_log` (
  `id` BIGINT(20) NOT NULL AUTO_INCREMENT,
  `pid` INT(11) NOT NULL,
  `log_level` INT(11) NULL DEFAULT '0',
  `message` VARCHAR(2000) NULL DEFAULT NULL COLLATE 'utf8_general_ci',
  PRIMARY KEY (`id`) USING BTREE
)
COLLATE='utf8_general_ci'
ENGINE=MyISAM;


create or replace view <table-prefix>scheduler_next as
  select 
        `<table-prefix>scheduler`.`id` AS `id`,
        `<table-prefix>scheduler`.`command` AS `command`,
        `<table-prefix>scheduler`.`running` AS `running`,
        `<table-prefix>scheduler`.`done` AS `done`,
        `<table-prefix>scheduler`.`scheduled_at` AS `scheduled_at`,
        `<table-prefix>scheduler`.`parameters` AS `parameters`,
        `<table-prefix>scheduler`.`RETURN_CODE` AS `RETURN_CODE`, 
        `<table-prefix>scheduler`.`resched_timescale` AS `resched_timescale`,
        `<table-prefix>scheduler`.`resched_value` AS `resched_value`
  from 
	      `<table-prefix>scheduler` 
 where 
         `<table-prefix>scheduler`.`done` = 0
    and  `<table-prefix>scheduler`.`scheduled_at` < now()
    and  `<table-prefix>scheduler`.`running` = 0

```


## notes on "table-prefix"scheduler

Fields **resched_timescale** and **resched_value** are used to schedulate again a process during execution.
**resched_timescale** identifies the type of value represented by **resched_value**.

Values for **resched_timescale**:

- Y = years
- M = months
- D = days
- H = hours
- N = minutes
- S = seconds

Example:
If **resched_timescale** is "H" and **resched_value** is 8, then current process will be scheduled again in 8 hours.







[more info](./docs/index.md)