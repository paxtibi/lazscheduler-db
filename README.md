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
```

