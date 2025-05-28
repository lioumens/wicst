
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

SELECT * INTO #pastyield
FROM (
	select plot_id, harvest_date as date, year(harvest_date) as year,
		dry_matter_tons_per_adjusted_acre as yield,
		method = 'harvest',
		cycle
	from #harvestpast
	where year(harvest_date) between 1990 and 2023
	UNION ALL
	select plot_id, biomass_date as date, year(biomass_date) as year,
	    dmta as yield,
	    method,
	    cycle
	from #biopast
) as cb;

-- imputed version of combined pasture yields
select * into #pastyield_imputed
from (select plot_id, harvest_date as date, year(harvest_date) as year,
		coalesce(dry_matter_tons_per_adjusted_acre, guess_dry_matter_tons_per_adjusted_acre) as yield,
		method = 'harvest',
		cycle
	from #harvestpast
	where year(harvest_date) between 1990 and 2023
	UNION ALL
	select plot_id, biomass_date as date, year(biomass_date) as year,
	    dmta as yield,
	    method,
	    cycle
	from #biopast
) as cb;

select * from #harvestpast;

--drop table #pastyield
--drop table #pastyield_imputed

select * from #pastyield order by date, plot_id;

-- assumes all on same date are subsamples
---- with harvest versions
select 
	date, year,	plot_id, cycle,
	avg_yield = AVG(yield),
	max_yield = MAX(yield)
into #pastyield_lvl1
from #pastyield
where method IN ('quadrat', 'harvest')
group by year, date, plot_id, cycle; -- cannot use alias in group_by statement

-- impute pasture at .6?

--drop table #pastyield_lvl1;

select * from #pastyield_lvl1

-- agg cycle
select year, plot_id, cycle,
avg_avg_yield = AVG(avg_yield),
max_max_yield = MAX(max_yield)
into #pastyield_lvl2
from #pastyield_lvl1
group by year, plot_id, cycle

--drop table #pastyield_lvl2;

select * from #pastyield_lvl2;

-- sum across cycles
select year, plot_id,
sum_avg_avg_yield = SUM(avg_avg_yield),
sum_max_max_yield = SUM(max_max_yield)
into #pastyield_lvl3
from #pastyield_lvl2
group by year, plot_id;

select * from #pastyield_lvl3
order by year, plot_id;

--drop table #pastyield_lvl3

-- without harvest
select 
	date, year,	plot_id, cycle,
	avg_yield = AVG(yield),
	max_yield = MAX(yield)
into #pastyield_lvl1_noharvest
from #pastyield
where method = 'quadrat'
group by year, date, plot_id, cycle; -- cannot use alias in group_by statement

select * from #pastyield_lvl1_noharvest; 

--drop table #pastyield_lvl1_noharvest;

-- agg cycle
select year, plot_id, cycle,
avg_avg_yield = AVG(avg_yield),
max_max_yield = MAX(max_yield)
into #pastyield_lvl2_noharvest
from #pastyield_lvl1_noharvest
group by year, plot_id, cycle

select * from #pastyield_lvl2_noharvest;

--drop table #pastyield_lvl2_noharvest;

-- sum across cycles
select year, plot_id,
sum_avg_avg_yield = SUM(avg_avg_yield),
sum_max_max_yield = SUM(max_max_yield)
into #pastyield_lvl3_noharvest 
from #pastyield_lvl2_noharvest
group by year, plot_id;

--select * from #pastyield_lvl3_noharvest
--where year = 2008
--order by year, plot_id;


--drop table #pastyield_lvl3_noharvest

--drop table #pastyield_lvl3_noharvest, #pastyield_lvl2_noharvest, #pastyield_lvl1_noharvest

----------------------
-- sum the yields over the exclosures
----------------------

--exclosures and harvests
select 
	year, plot_id, -- exclosures do not have cycle info
	sum_yield = SUM(yield),
	count_harvestexclosures = count(yield)
into #pastyield_lvl1_harvestexclosures
from #pastyield
where method IN ('harvest', 'exclosure')
group by year, plot_id;

--drop table #pastyield_lvl1_harvestexclosures

-- just exclosures
select 
	year, plot_id, -- exclosures do not have cycle info
	sum_yield = SUM(yield),
	count_exclosures = count(yield)
into #pastyield_lvl1_exclosures
from #pastyield
where method IN ('exclosure')
group by year, plot_id;

-- impute pasture moisture exclosures .6 for 2015
select
	year, plot_id,
	sum_yield = SUM(yield),
	count_harvestexclosures = count(yield)
into #pastyield_imputed_lvl1_harvestexclosures
from #pastyield_imputed
where method IN ('harvest', 'exclosure')
group by year, plot_id;

select
	year, plot_id,
	sum_yield = SUM(yield),
	count_exclosures = count(yield)
into #pastyield_imputed_lvl1_exclosures
from #pastyield_imputed
where method IN ('exclosure')
group by year, plot_id;


select * from #pastyield_imputed_lvl1_harvestexclosures
where year = 2023

--drop table #pastyield_imputed_lvl1_harvestexclosures

--TODO: left off on trying to combine this all into one table, for each year

select * from #pastyield;

select * from #pastyield_lvl1_exclosures
where year = 2015
order by year, plot_id;

select * from #pastyield_lvl1_harvestexclosures
where year = 2015
order by year, plot_id;

select * from #pastyield
where year = 2015

--drop table #pastyield_lvl1_harvestexclosures;
--drop table #pastyield_lvl1_exclosures

select * from #pastyield_lvl3_noharvest

select * from #pastyield_lvl3 order by year, plot_id

-- add harvest/biomass/exclosure info
with base as (
select year, plot_id, method, count(*) as n from #pastyield
group by plot_id, year, method
)
SELECT [year], [plot_id], ISNULL([harvest], 0) as 'harvest', ISNULL([quadrat], 0) as 'quadrat', ISNULL([exclosure], 0) from base 
PIVOT (
	max(n)
	FOR [method] in ([harvest], [quadrat], [exclosure])
) as pt
order by year, plot_id


select year, num_days, count(*) as n,
row_number()
from wicst.grazing_summary
group by year, num_days
order by year, n DESC

select year, num_days, count(num_days) OVER(PARTITION BY year) from wicst.grazing_summary

select * from wicst.pasture_yield_summary
order by year, plot_id

with harvestpast AS (
	select hs.*, hd.tenday, hd.cycle
	from wicst.harvesting_summary hs
	left join wicst.harvestingdetails hd on hs.harvesting_id = hd.harvesting_id
	where product = 'pasture' and (harvest_lbs  > 0 or harvest_lbs IS NULL)
), biopast AS (
	select b.*, bd.cycle,
	--TODO: biomass yield, not accounting for loss in the pastures
	-- dry matter tons acre
	dmta = biomass_grams / 1000 * common.CONST_KG_TO_LBS() / 2000 * (100 - percent_moisture) / 100 / (biomass_area / common.CONST_ACRE_TO_FT2())
	from wicst.biomassings b
	left join wicst.biomassingdetails bd on b.biomassing_id = bd.biomassing_id
	where biomass = 'pasture' and year(biomass_date) between 1990 and 2023 --TODO remove year concstaints for for generalizability
)
select * from biopast;


SELECT COLUMN_NAME FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = 'pasture_yield_summary' ORDER BY ORDINAL_POSITION

