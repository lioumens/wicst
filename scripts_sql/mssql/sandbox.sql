
-- A little ugly, but gets the job done

CREATE VIEW wicst.LookupCoreTreatment AS
/*
 * Recreation of the treatment rotation grid for the core WICST trial.
 */
WITH maxphase AS (
    SELECT 
        system_id, 
        MAX(phase) AS phases
    FROM wicst.rotations
    GROUP BY system_id
), yearseq AS (
    SELECT 
        theyear = n
    FROM (SELECT TOP (2100 - 1989 + 1) 
                 ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) + 1988 AS n
          FROM master.dbo.spt_values) AS years
), offs AS (
	select plot_id, block, theyear, start_year, theyear - (start_year - 1) as offf, system_id, p.treatment_id from yearseq 
	cross join plots p
	left join treatments t on p.treatment_id  = t.treatment_id
), phases as (
	select plot_id, block, theyear, treatment_id, offs.system_id, start_year, CAST(offf AS INT) % CAST(phases AS INT) as phase from offs
	left join maxphase on offs.system_id = maxphase.system_id
), plants as (
	select plot_id, block, theyear, start_year, p.system_id, treatment_id, p.phase + 1 as phase, crop from phases p
	left join rotations r on p.system_id = r.system_id and p.phase + 1 = r.phase 
)
select theyear as year, ,
case
  when theyear < start_year then 'fc'
  else crop
end as realcrop
from plants;

-- temporary testing
with treatment_wide AS (
select plot_id, block, treatment_id, [2023] as TRT_2023, [2024] as TRT_2024
from (select plot_id, block, treatment_id, theyear, realcrop from wicst.treatment_lookup) as sourcetable
pivot
(
	MAX(realcrop) 
	for theyear in ([2023], [2024])
) as pivottable
where plot_id like 'A%')
select treatment_id, [1] as "Rep 1", [2] as "Rep 2", [3] as "Rep 3", [4] as "Rep 4", TRT_2023, TRT_2024
from (select plot_id, treatment_id, block, trt_2023, trt_2024 from treatment_wide) as steptable
pivot
(
	MAX(plot_id)
	for block in ([1], [2], [3], [4])
) as pivotted
order by treatment_id asc;

select * from wicst.treatment_lookup;

-- create a dynamic version with start year
DECLARE @cols NVARCHAR(MAX), @query NVARCHAR(MAX), @startyear SMALLINT, @endyear SMALLINT;
SET @startyear = 1989;
SET @endyear = 2050;

-- can create dynamic version
select @cols = STRING_AGG(quotename(theyear), ',')
from (select distinct theyear from wicst.LookupCoreTreatment
where theyear between @startyear and @endyear) as years;

set @query = '
CREATE VIEW wicst.LookupCoreTreatmentWidest AS
with treatment_wide as 
(
	select plot_id, block, treatment_id, ' + @cols + '
	from (select plot_id, block, treatment_id, theyear, realcrop from wicst.LookupCoreTreatment) as sourcetable
	pivot
	(
		MAX(realcrop)
		for theyear in (' + @cols + ')
	) as pivottable
	where plot_id like ''A%''
)
select treatment_id, [1] as "Rep 1", [2] as "Rep 2", [3] as "Rep 3", [4] as "Rep 4",' + @cols + '
from  (select plot_id, treatment_id, block,' + @cols + ' from treatment_wide) as steptable
pivot
(
	MAX(plot_id)
    for block in ([1], [2], [3], [4])
) as pivotted;';

EXEC sp_executesql @query;


-- using a stored proceduree for updating
CREATE PROCEDURE MAKE_TREATMENT_LOOKUP 
	@endyear INT = 2100,
  	@startyear INT = 1989
AS 
BEGIN
	DECLARE @cols NVARCHAR(MAX), @query NVARCHAR(MAX)
	SELECT @cols = STRING_AGG(quotename(theyear), ',')
	FROM (
		SELECT DISTINCT theyear from wicst.LookupCoreTreatment
		WHERE theyear BETWEEN @startyear and @endyear
	) as years;

	
	set @query = '
		CREATE VIEW wicst.LookupCoreTreatmentWidest AS
		with treatment_wide as 
		(
			select plot_id, block, treatment_id, ' + @cols + '
			from (select plot_id, block, treatment_id, theyear, realcrop from wicst.LookupCoreTreatment) as sourcetable
			pivot
			(
				MAX(realcrop)
				for theyear in (' + @cols + ')
			) as pivottable
			where plot_id like ''A%''
		)
		select treatment_id, [1] as "Rep 1", [2] as "Rep 2", [3] as "Rep 3", [4] as "Rep 4",' + @cols + '
		from  (select plot_id, treatment_id, block,' + @cols + ' from treatment_wide) as steptable
		pivot
		(
			MAX(plot_id)
		    for block in ([1], [2], [3], [4])
		) as pivotted;';
	
	DROP VIEW IF EXISTS wicst.LookupCoreTreatmentWidest;
	EXEC sp_executesql @query;
END;
--drop procedure make_treatment_lookup;

-- cool
EXEC MAKE_TREATMENT_LOOKUP 2100;


SELECT ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) + 1988 AS n from Customers c;
SELECT ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) + 1988 AS n from Customers c;

select top 5 * from master.dbo.spt_values; -- can use for random things

select number n from master.dbo.spt_values where type = 'P' and number between 10 and 20;


select * from Customers c;

WITH cte as ( select 1 as RowNum union all select RowNum+1 from cte where RowNum<100 ) select * from cte;


-- Generates number vectors of max lenghth 10000
DROP PROCEDURE if exists SEQ;

CREATE PROCEDURE SEQ
  	@end REAL,
	@start REAL = 1,
	@by REAL = 1
AS
BEGIN
	with nums as ( select @start as num union all select num + @by from nums where num < @end) select * from nums where num < @end
	OPTION (MAXRECURSION 10000);
END;
--EXEC SEQ  @start = 1, @end = 9, @by = 3.2;

-- GET SILAGE

select @@version;

SELECT *, dbo.CONST_IDEAL_MOISTURE ('corn') as ideal_moisture,
dry_matter_lbs,
dry_matter_tons,
ROUND(dm_per_acre, 4) as dm_per_acre
from wicst.harvestings
-- cross apply in order to select calculated columns rowwise
-- calculated once
cross apply (
	select 
	(100 - percent_moisture) / 100 * harvest_lbs as dry_matter_lbs,
	(100 - percent_moisture) / 100 * harvest_lbs / 2000 as dry_matter_tons,
	(100 - percent_moisture) / 100 * harvest_lbs / 2000 / (harvest_area / dbo.CONST_ACRE_TO_FT2()) as dm_per_acre
	) as calc
where year(harvest_date) = '2022' AND product = 'corn';

CREATE PROCEDURE GET_TONS_PER_ACRE
	@product VARCHAR
AS
BEGIN
	select * from wicst.harvestings
	where year(harvest_date) = '2022'
END;

EXEC GET_TONS_PER_ACRE @product = 'corn';

select dbo.CONST_IDEAL_MOISTURE('corn') as ideal_moisture;

select plot_id, calc.ideal_corrected_bu_per_acre from wicst.harvestings
cross apply(
	select 
	(100 - percent_moisture) / 100 * harvest_lbs as dry_matter_lbs,
	(100 - percent_moisture) / 100 * harvest_lbs / 2000 as dry_matter_tons,
	(harvest_area / dbo.CONST_ACRE_TO_FT2()) as acre_frac,
	harvest_lbs / (harvest_area / dbo.CONST_ACRE_TO_FT2()) as lbs_per_acre,
	dbo.CONST_IDEAL_MOISTURE('corn') as ideal_moisture,
	(100 - percent_moisture) / (100 - dbo.CONST_IDEAL_MOISTURE('corn')) as moisture_correction,
	(100 - percent_moisture) / (100 - dbo.CONST_IDEAL_MOISTURE('corn')) * harvest_lbs as corrected_lbs,
	(100 - percent_moisture) / (100 - dbo.CONST_IDEAL_MOISTURE('corn')) * harvest_lbs / (harvest_area / dbo.CONST_ACRE_TO_FT2()) as corrected_lbs_per_acre,
	dbo.CONST_IDEAL_BUSHEL('corn') as ideal_bushel,
	harvest_lbs / (harvest_area / dbo.CONST_ACRE_TO_FT2()) / dbo.CONST_IDEAL_BUSHEL('corn') as ideal_bu_per_acre,
	(100 - percent_moisture) / (100 - dbo.CONST_IDEAL_MOISTURE('corn')) * harvest_lbs / (harvest_area / dbo.CONST_ACRE_TO_FT2()) / dbo.CONST_IDEAL_BUSHEL('corn') as ideal_corrected_bu_per_acre,
	(100 - percent_moisture) / 100 * harvest_lbs / 2000 / (harvest_area / dbo.CONST_ACRE_TO_FT2()) as dm_per_acre
) as calc
where product = 'corn' and year(harvest_date)='2022';



-- example table for testing the inclusion exclusion table calculation
create table #syslosstest (
	harvesting_id varchar(255),
	loss_fraction float,
	loss_category varchar(255))
	
insert into  #syslosstest (harvesting_id, loss_fraction, loss_category)
VALUES
('A', .1, 'weeds'),
('A', .02, 'weeds'),
('A', .2, 'lodging'),
('B', .02, 'weeds'),
('B', .3, 'lodging'),
('B', .073, 'storm'),
('B', .133, 'water'),
('B', .05, 'water');

declare @query varchar(max) = '';
-- this is close, but i don't want to run sql from the output, seems dangerous, and difficult
select harvesting_id, (STUFF((
    SELECT CASE WHEN set_size % 2 = 1 THEN '+' ELSE '-' END + CAST(sys_loss as VARCHAR(MAX))
	from #sysintersections as s
	where s.harvesting_id = t.harvesting_id
	order by set_size
	for XML PATH(''), TYPE
	).value('.', 'varchar(max)')
, 1, 1, ''))
from #sysintersections as t
group by harvesting_id;


-- mtcars, does updating table mess up cascade?
select * from mtcars;

-- on delete cascade behavior
CREATE TABLE ParentTable (
    ParentID INT PRIMARY KEY,
    ParentName NVARCHAR(50)
);

CREATE TABLE ChildTable (
    ChildID INT PRIMARY KEY,
    ParentID INT,
    ChildName NVARCHAR(50),
    FOREIGN KEY (ParentID) REFERENCES ParentTable(ParentID) ON DELETE CASCADE
);

-- Insert data into ParentTable
INSERT INTO ParentTable (ParentID, ParentName)
VALUES (1, 'Parent A'), (2, 'Parent B');

-- Insert data into ChildTable
INSERT INTO ChildTable (ChildID, ParentID, ChildName)
VALUES (1, 1, 'Child A1'), (2, 1, 'Child A2'), (3, 2, 'Child B1');

SELECT * from ParentTable
SELECT * from ChildTable; -- triggers the cascade delete

delete from parenttable;

select

---
-- get yield information from product
---
 
SELECT TOP 54 
                 ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) + 1988 AS n
          FROM master.dbo.spt_values AS years;

select * from wicst.treatment_lookup;
drop view if exists wicst.treatment_lookup;

select p.plot_id, block, system_id, p.treatment_id, year, 
case
	when year < start_year then 'fc'
	else crop
end
from plants l
left join plots p on p.plot_id = l.plot_id;
