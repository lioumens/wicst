
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


---
-- Calculate harvest yields for pasture


-- need to ensure harvestings have the cycle information

-- combine harvesting and biomassing information
select hs.*, hd.tenday, hd.cycle
into #harvestpast
from wicst.harvesting_summary hs
left join wicst.harvestingdetails hd on hs.harvesting_id = hd.harvesting_id
where product = 'pasture' and (harvest_lbs  > 0 or harvest_lbs IS NULL);

select b.*, bd.cycle,
dmta = biomass_grams / 1000 * common.CONST_KG_TO_LBS() / 2000 * (100 - percent_moisture) / 100 / (biomass_area / common.CONST_ACRE_TO_FT2()) -- biomass yield, not accounting for loss yet
into #biopast
from wicst.biomassings b
left join wicst.biomassingdetails bd on b.biomassing_id = bd.biomassing_id
where biomass = 'pasture' and year(biomass_date) between 1990 and 2023;

--select * from #biopast
--
--drop table #biopast
--

select distinct year(biomass_date) as year from wicst.biomassings
order by year;

-- null value
-- 2014 plots missing

drop table #harvestpast

-- combined bio/harvest


--drop table #pastyield_imputed_lvl1_harvestexclosures

-- const table


select num as year, lowernum
from common.SEQ(2023, 1990, 1)
cross apply (select lowernum = num - 5) as other

-- start with this, and left join away
with harvestpast AS (
	select hs.*, hd.tenday, hd.cycle
	from wicst.harvesting_summary hs
	left join wicst.harvestingdetails hd on hs.harvesting_id = hd.harvesting_id
	where product = 'pasture' and (harvest_lbs  > 0 or harvest_lbs IS NULL)
), biopast AS (
	select b.*, bd.cycle,
	--TODO: biomass yield, not accounting for loss in the pastures
	dmta = biomass_grams / 1000 * common.CONST_KG_TO_LBS() / 2000 * (100 - percent_moisture) / 100 / (biomass_area / common.CONST_ACRE_TO_FT2())
	from wicst.biomassings b
	left join wicst.biomassingdetails bd on b.biomassing_id = bd.biomassing_id
	where biomass = 'pasture' and year(biomass_date) between 1990 and 2023 -- remove later for generalizability
), pastyield AS (
	SELECT *
	FROM (
		select plot_id, harvest_date as date, year(harvest_date) as year,
			dry_matter_tons_per_adjusted_acre as yield,
			method = 'harvest',
			cycle
		from harvestpast
		where year(harvest_date) between 1990 and 2023
		UNION ALL
		select plot_id, biomass_date as date, year(biomass_date) as year,
		    dmta as yield,
		    method,
		    cycle
		from biopast
	) as cb
), pastyield_imputed AS (
	select *
	from (select plot_id, harvest_date as date, year(harvest_date) as year,
			coalesce(dry_matter_tons_per_adjusted_acre, guess_dry_matter_tons_per_adjusted_acre) as yield,
			method = 'harvest',
			cycle
		from harvestpast
		where year(harvest_date) between 1990 and 2023
		UNION ALL
		select plot_id, biomass_date as date, year(biomass_date) as year,
		    dmta as yield,
		    method,
		    cycle
		from biopast
	) as cb
), 
-- Three levels of sum/averaging/maxing for quadrat + harvesting
-- 1. avg/max subsample
-- 2. avg/max cycle
-- 3. sum plot
pastyield_lvl1 AS (
	select 
		date, year,	plot_id, cycle,
		avg_yield = AVG(yield),
		max_yield = MAX(yield)
	from pastyield
	where method IN ('quadrat', 'harvest')
	group by year, date, plot_id, cycle
), pastyield_lvl2 AS (
	select year, plot_id, cycle,
	avg_avg_yield = AVG(avg_yield),
	max_max_yield = MAX(max_yield)
	from pastyield_lvl1
	group by year, plot_id, cycle
), pastyield_lvl3 AS (
	select year, plot_id,
	sum_avg_avg_yield = SUM(avg_avg_yield),
	sum_max_max_yield = SUM(max_max_yield)
	from pastyield_lvl2
	group by year, plot_id
), pastyield_lvl1_noharvest AS (
	select 
		date, year,	plot_id, cycle,
		avg_yield = AVG(yield),
		max_yield = MAX(yield)
	from pastyield
	where method = 'quadrat'
	group by year, date, plot_id, cycle
), pastyield_lvl2_noharvest AS (
	select year, plot_id, cycle,
	avg_avg_yield = AVG(avg_yield),
	max_max_yield = MAX(max_yield)
	from pastyield_lvl1_noharvest
	group by year, plot_id, cycle
), pastyield_lvl3_noharvest AS (
	select year, plot_id,
	sum_avg_avg_yield = SUM(avg_avg_yield),
	sum_max_max_yield = SUM(max_max_yield)
	from pastyield_lvl2_noharvest
	group by year, plot_id
), pastyield_lvl1_harvestexclosures AS (
	select 
		year, plot_id, -- exclosures do not have cycle info
		sum_yield = SUM(yield),
		count_harvestexclosures = count(yield)
	from pastyield
	where method IN ('harvest', 'exclosure')
	group by year, plot_id
), pastyield_lvl1_exclosures AS (
	select 
		year, plot_id, -- exclosures do not have cycle info
		sum_yield = SUM(yield),
		count_exclosures = count(yield)
	from pastyield
	where method IN ('exclosure')
	group by year, plot_id
), pastyield_imputed_lvl1_harvestexclosures AS (
	select
		year, plot_id,
		sum_yield = SUM(yield),
		count_harvestexclosures = count(yield)
	from pastyield_imputed
	where method IN ('harvest', 'exclosure')
	group by year, plot_id
), pastyield_imputed_lvl1_exclosures AS (
	select
		year, plot_id,
		sum_yield = SUM(yield),
		count_exclosures = count(yield)
	from pastyield_imputed
	where method IN ('exclosure')
	group by year, plot_id
), pasture_year_plot AS (
	-- outer product of years and pasture plots
	select 
		num as year,	
		pasture_plots.plot_id
	from common.SEQ(2023, 1990, 1)
	cross join (
		select *
		from (VALUES ('A112'),('A207'),('A302'),('A405')) as ConstTable(plot_id)
	) as pasture_plots
)
select 
	pyp.*,
	h.sum_avg_avg_yield as sum_avg_avg_yield_harvestquadrat,
	h.sum_max_max_yield as sum_max_max_yield_harvestquadrat,
	nh.sum_avg_avg_yield as sum_avg_avg_yield_quadrat,
	nh.sum_max_max_yield as sum_max_max_yield_quadrat,
	he.sum_yield as sum_yield_harvestexclosures,
	e.sum_yield as sum_yield_exclosures,
	ihe.sum_yield as sum_imputed_yield_harvestexclosures,
	ie.sum_yield as sum_imputed_yield_exclosures
from pasture_year_plot pyp
left join pastyield_lvl3 h on pyp.year = h.year and pyp.plot_id = h.plot_id
left join pastyield_lvl3_noharvest nh on pyp.year = nh.year and pyp.plot_id = nh.plot_id
left join pastyield_lvl1_harvestexclosures he on pyp.year = he.year and pyp.plot_id = he.plot_id
left join pastyield_lvl1_exclosures e on pyp.year = e.year and pyp.plot_id = e.plot_id
left join pastyield_imputed_lvl1_harvestexclosures ihe on pyp.year = ihe.year and pyp.plot_id = ihe.plot_id
left join pastyield_imputed_lvl1_exclosures ie on pyp.year = ie.year and pyp.plot_id = ie.plot_id
order by year, plot_id

select * from #pastyield_lvl1_harvestexclosures
select * from #pastyield_lvl1_exclosures
select * from #pastyield_imputed_lvl1_harvestexclosures

--select distinct plot_id from wicst.LookupCoreTreatment
--where realcrop = 'P' and LEFT(plot_id, 1) = 'A'

select 
coalesce(h.year, nh.year) as year,
coalesce(h.plot_id, nh.plot_id) as plot_id,
nh.sum_avg_avg_yield as sum_avg_avg_yield_no_harvest,
nh.sum_max_max_yield as sum_max_max_yield_no_harvest,
--he.sum_yield as sum_yield_harvest_and_exclosures,
h.sum_avg_avg_yield, h.sum_max_max_yield
from #pastyield_lvl3_noharvest nh
full join #pastyield_lvl3 h on nh.year = h.year and nh.plot_id = h.plot_id
--full join #pastyield_lvl1_harvestexclosures he on he.year = coalesce(h.year, nh.year) and he.plot_id = coalesce(h.plot_id, nh.plot_id)
order by year, plot_id;

select * from wicst.biomassings b
left join wicst.biomassingdetails bd on b.biomassing_id =  bd.biomassing_id
where method = 'quadrat' and year(biomass_date) = 2008;


select * from wicst.harvestings
where year(harvest_date) = 2008 and product = 'pasture'


select 
ca.average_weight / ca.num_days as adg
from wicst.grazings
cross apply (
	select 
	num_days = DATEDIFF(day, on_date, off_date),
	average_weight = end_lbs - start_lbs
) as ca;



select * from wicst.harvestings h
left join wicst.harvestingdetails hd on h.harvesting_id = hd.harvesting_id
where product = 'pasture' AND year(harvest_date) = 2013;











