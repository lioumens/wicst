
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
select *,
case
  when theyear < start_year then 'fc'
  else crop
end as realcrop
from plants;

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

CREATE FUNCTION dbo.SEQ (
/*
 * Convenience function to generate sequence of numbers.
 */
    @end REAL,
    @start REAL = 1,
    @by REAL = 1
)
RETURNS @Nums TABLE (num REAL)
AS
BEGIN
    DECLARE @current REAL = @start;

    WHILE @current <= @end
    BEGIN
        INSERT INTO @Nums (num)
        VALUES (@current);

        SET @current = @current + @by;
    END
    
    RETURN;
END;

-- trt for silage view
CREATE VIEW wicst.LookupSilageTreatmentWidest AS
with b as (
select * from wicst.plots
	where left(plot_id, 1 ) = 'A'),
a AS (
SELECT num as year, 
	   CAST((num 	- 2017) AS INT) % 4 + 7 as cs4_trt,
	   CAST((num - 2017 + 1) AS INT) % 3 + 11 as cs5_trt
	   from dbo.seq(2050, 2018, 1)),
c as (
SELECT pivotted.year, cs4_trt, cs5_trt, [1] as "CS4 Rep 1", [2] as "CS4 Rep 2", [3] as "CS4 Rep 3", [4] as "CS4 Rep 4" from a
left join b
on a.cs4_trt = b.treatment_id
PIVOT
(
	MAX(plot_id)
	for block in ([1],[2],[3],[4])
) as pivotted
)
SELECT pivot2.year, cs4_trt, cs5_trt, "CS4 Rep 1", "CS4 Rep 2", "CS4 Rep 3", "CS4 Rep 4", [1] as "CS5 Rep 1", [2] as "CS5 Rep 2", [3] as "CS5 Rep 3", [4] as "CS5 Rep 4" from c
left join b
on c.cs5_trt = b.treatment_id
PIVOT
(
	MAX(plot_id)
	for block in ([1],[2],[3],[4])
) as pivot2;


CREATE VIEW wicst.LookupSilageTreatment AS
select year, sys_rep, plot_id from wicst.LookupSilageTreatmentWidest
UNPIVOT 
(
	plot_id for sys_rep in ("CS4 Rep 1", "CS4 Rep 2", "CS4 Rep 3", "CS4 Rep 4", 
							"CS5 Rep 1", "CS5 Rep 2", "CS5 Rep 3", "CS5 Rep 4")
) as unpivoted;

-- GET SILAGE
CREATE FUNCTION dbo.CONST_IDEAL_MOISTURE (@product VARCHAR(50))
  /**
   * Returns Ideal Moisture for adjusted yields for products
   * 
   * @param product - The first input number
   * @returns moisture percentage
   */
RETURNS REAL AS 
BEGIN
	DECLARE @IDEAL_MOISTURE REAL
	SET @IDEAL_MOISTURE = 
		CASE @product
			WHEN 'corn' THEN 15.5
			WHEN 'soybean' THEN 13
			WHEN 'barley' THEN 14.5
			WHEN 'wheat' THEN 13.5
			WHEN 'wheat grain' THEN 13.5
			WHEN 'oat' THEN 14
			else NULL
		END;
RETURN @IDEAL_MOISTURE
END;

CREATE FUNCTION dbo.CONST_IDEAL_BUSHEL (@product VARCHAR(50))
  /**
   * Returns Ideal pounds per bushel for adjusted yields for products
   * 
   * @param product - The first input number
   * @returns ideal pounds per bushel
   */
RETURNS REAL
AS 
BEGIN
	DECLARE @IDEAL_BUSHEL REAL
	SET @IDEAL_BUSHEL = 
	CASE @product
	WHEN 'corn' THEN 56
	WHEN 'soybean' THEN 60
	WHEN 'barley' THEN 48
	WHEN 'wheat' THEN 60
	WHEN 'wheat grain' THEN 60
	WHEN 'wheat straw' THEN 60
	WHEN 'oat' THEN 32
	else 0
	END;
RETURN @IDEAL_BUSHEL
END;


-- CONSTANT DEFINITIONS
CREATE FUNCTION dbo.CONST_ACRE_TO_FT2() RETURNS INT AS
BEGIN 
	RETURN 43560
END;

CREATE FUNCTION dbo.CONST_M_TO_FT() RETURNS FLOAT AS
BEGIN
	RETURN 3.28084
END;

CREATE FUNCTION dbo.CONST_M2_TO_FT2() RETURNS FLOAT AS
BEGIN
	RETURN POWER(dbo.CONST_M_TO_FT(), 2)
END;

CREATE FUNCTION dbo.CONST_KG_TO_LBS() RETURNS FLOAT AS
AS BEGIN
	RETURN 2.2046226
END;

SELECT *, dbo.CONST_IDEAL_MOISTURE ('corn') as ideal_moisture,
dry_matter_lbs,
dry_matter_tons,
ROUND(dm_per_acre, 4) as dm_per_acre
from wicst.harvestings
cross apply (
	select 
	percent_moisture / 100 * harvest_lbs as dry_matter_lbs,
	percent_moisture / 100 * harvest_lbs / 2000 as dry_matter_tons,
	(100 - percent_moisture) / 100 * harvest_lbs / 2000 / (harvest_area / dbo.CONST_ACRE_TO_FT2()) as dm_per_acre
	) as calc
where year(harvest_date) = '2022' AND crop LIKE '%[Cc]orn%';

CREATE PROCEDURE GET_TONS_PER_ACRE
	@product VARCHAR
AS
BEGIN
	select * from wicst.harvestings
	where year(harvest_date) = '2022'
END;

EXEC GET_TONS_PER_ACRE @product = 'hello';

select * from wicst.harvestings h
where year(harvest_date) = '2022' AND crop LIKE '%[Cc]orn%';


select 1 as RowNum from wicst.rotations;

select 
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
