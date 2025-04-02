/*
 * Depends: constant.sql
 */

-------------------------
--   WICST Grid Views  --
-------------------------

-- pivot Lookuptable to show 4 plots per row
--drop procedure make_treatment_lookup; -- if needed
CREATE PROCEDURE MAKE_TREATMENT_LOOKUP 
	@endyear INT = 2100,
  	@startyear INT = 1989
AS 
BEGIN
	/*
	 *  Long Version
	 * 
	 *  Recreation of the treatment rotation grid for the core WICST trial. Currently the implementatio is a little ugly
	 */
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
	          FROM master.common.spt_values) AS years
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
	
	/*
	 *  Wide Version
	 */

	DECLARE @cols NVARCHAR(MAX), @query NVARCHAR(MAX)
	SELECT @cols = STRING_AGG(quotename(theyear), ',')
	FROM (
		SELECT DISTINCT theyear from wicst.LookupCoreTreatment -- depends on previous View create to pivot wider
		WHERE theyear BETWEEN @startyear and @endyear
	) as years;

	-- must be dynamic query to make use of sequence between @startyear and @endyear
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
	
	-- replace existing view
	DROP VIEW IF EXISTS wicst.LookupCoreTreatmentWidest;
	EXEC sp_executesql @query;
END;

-- create the wider view
EXEC MAKE_TREATMENT_LOOKUP 2100;

-------------------------
-- Silage Grid Views --
-------------------------

-- trt for silage view
CREATE VIEW wicst.LookupSilageTreatmentWidest AS
/* 
 * Wider version of Silage Rotation Treatments
 */
with b as (
select * from wicst.plots
	where left(plot_id, 1 ) = 'A'),
a AS (
SELECT num as year, 
	   CAST((num 	- 2017) AS INT) % 4 + 7 as cs4_trt,
	   CAST((num - 2017 + 1) AS INT) % 3 + 11 as cs5_trt
	   from common.seq(2050, 2018, 1)),
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
/* 
 * Longer version of Silage Rotation Treatments
 */
SELECT year, sys_rep, plot_id from wicst.LookupSilageTreatmentWidest
UNPIVOT 
(
	plot_id for sys_rep in ("CS4 Rep 1", "CS4 Rep 2", "CS4 Rep 3", "CS4 Rep 4", 
							"CS5 Rep 1", "CS5 Rep 2", "CS5 Rep 3", "CS5 Rep 4")
) as unpivoted;



-----------------
-- Yield Views --
-----------------
select * from wicst.harvesting_summary hs;

-- working to calculate total loss fraction and loss table
--TODO: will need to change these tables to be yielding ID's
CREATE VIEW wicst.harvesting_summary as
/* 
 * These harvest numbers account for direct loss and systematic loss
 */
WITH aggs AS (
	select 
		harvesting_id, 
		loss_category,
		SUM(loss_fraction) as sys_loss,
		CAST(loss_category AS NVARCHAR(MAX)) as set_name,
		1 as set_size
	from wicst.systematiclosses s
	group by harvesting_id, loss_category -- group by is not allows in recursive union step, so aggregate first
	),
	intersections as (
	-- recursively join for creating all intersection sets
	select * from aggs
	UNION ALL 
		SELECT
			a.harvesting_id, a.loss_category,
			a.sys_loss * i.sys_loss, -- assume independence between loss categories
			CAST(i.set_name + ' + ' + a.loss_category AS NVARCHAR(MAX)) as set_name, -- save set name for debugging
			i.set_size + 1 as set_size
		FROM aggs a
		INNER JOIN intersections i ON (a.harvesting_id = i.harvesting_id AND a.loss_category < i.loss_category)
	),
	signed as (
	   select harvesting_id, loss_category,
	   case when set_size % 2 = 1 then +sys_loss else -sys_loss END AS signed_sys_loss -- flip sign according to inclusion/exclusion formula
	   from intersections
	),
	combined_system_losses as (
	    /*
	     * Aggregate the system losses table
	     */
		select harvesting_id, sum(signed_sys_loss) as total_loss_fraction from signed
		group by harvesting_id
	),
	combined_direct_losses as (
		/*
		 * Aggregate the direct losses table
		 */
		select harvesting_id, sum(loss_area) as total_loss_area from wicst.directlosses
		group by harvesting_id
	)
	select wp.block, wp.treatment_id, wt.system_id, h.*, s.total_loss_fraction, d.total_loss_area, calc.* from wicst.harvestings h
	left join combined_system_losses as s on h.harvesting_id = s.harvesting_id
	left join combined_direct_losses AS d ON h.harvesting_id = d.harvesting_id
	left join wicst.plots as wp on h.plot_id = wp.plot_id
	left join wicst.treatments as wt on wp.treatment_id = wt.treatment_id
	cross apply (
	select
	    year                  = YEAR(harvest_date),
		adjusted_harvest_area = (harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)), -- subtract direct area first, then probabilistic removed.
		dry_matter_lbs        = (100 - percent_moisture) / 100 * harvest_lbs,
		dry_matter_tons       = (100 - percent_moisture) / 100 * harvest_lbs / 2000,
		acre_frac             = (harvest_area / common.CONST_ACRE_TO_FT2()),
		lbs_per_acre          = harvest_lbs / (harvest_area / common.CONST_ACRE_TO_FT2()),
		ideal_moisture        = common.CONST_IDEAL_MOISTURE(product),
		moisture_correction         = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)),
		corrected_lbs               = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs,
		corrected_lbs_per_acre      = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs / (harvest_area / common.CONST_ACRE_TO_FT2()),
		corrected_lbs_per_adjusted_acre = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs / ((harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)) / common.CONST_ACRE_TO_FT2()),
		ideal_bushel                = common.CONST_IDEAL_BUSHEL(product),
		ideal_bu_per_acre           = harvest_lbs / (harvest_area / common.CONST_ACRE_TO_FT2()) / common.CONST_IDEAL_BUSHEL(product),
		ideal_bu_per_adjusted_acre  = harvest_lbs / ((harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)) / common.CONST_ACRE_TO_FT2()) / common.CONST_IDEAL_BUSHEL(product),
		ideal_corrected_bu_per_acre = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs / (harvest_area / common.CONST_ACRE_TO_FT2()) / common.CONST_IDEAL_BUSHEL(product),
		ideal_corrected_bu_per_adjusted_acre = (100 - percent_moisture) / (100 - common.CONST_IDEAL_MOISTURE(product)) * harvest_lbs / ((harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)) / common.CONST_ACRE_TO_FT2()) / common.CONST_IDEAL_BUSHEL(product),
		dry_matter_tons_per_acre             = (100 - percent_moisture) / 100 * harvest_lbs / 2000 / (harvest_area / common.CONST_ACRE_TO_FT2()),
		dry_matter_tons_per_adjusted_acre   = (100 - percent_moisture) / 100 * harvest_lbs / 2000 / ((harvest_area - coalesce(total_loss_area, 0)) * (1 - coalesce(total_loss_fraction, 0)) / common.CONST_ACRE_TO_FT2())
	) as calc

-- drop view wicst.harvesting_summary 
