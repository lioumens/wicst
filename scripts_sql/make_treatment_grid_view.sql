-- Defines two views:
--   treatment_grid
--   all_treatments

-- ugly, but gets the next rotation based on phases and system
create or replace view treatment_lookup as 
with maxphase as (
	select system_id, max(phase) as phases
	from rotations r
	group by system_id
), yearseq as (
    select generate_series(1989, 2042) as year
), offsets as (
	select plot_id, year, start_year, year - (start_year - 1) as off, system_id from yearseq 
	cross join plots p
	left join treatments t on p.treatment_id  = t.treatment_id
), phases as (
	select plot_id, year, offsets.system_id, start_year, off::INTEGER % phases::INTEGER as phase from offsets
	left join maxphase on offsets.system_id = maxphase.system_id
), plants as (
	select plot_id, year, start_year, p.system_id, p.phase + 1 as phase, crop from phases p
	left join rotations r on p.system_id = r.system_id and p.phase + 1 = r.phase 
)
select p.plot_id, block, system_id, p.treatment_id, year, 
case
	when year < start_year then 'fc'
	else crop
end
from plants l
left join plots p on p.plot_id = l.plot_id
order by plot_id asc, year asc;

create or replace function make_treatment_grid_view(end_year int, start_year int default 1989)
returns void as $$ 
DECLARE
    col_query text;
    row_query text;
	cols text;
	sql_query text;
BEGIN
	DROP VIEW IF EXISTS treatment_grid; -- drop needed if dropping columns

	SELECT string_agg(DISTINCT quote_ident(year::TEXT),' text,')
    INTO cols
    FROM treatment_lookup
	where year between start_year and end_year;

    row_query := format('select plot_id, block, system_id, treatment_id, year, crop from treatment_lookup where year between %s and %s', start_year, end_year);
    col_query := format('select generate_series(%s, %s)', start_year, end_year);
    sql_query := format('CREATE or REPLACE VIEW treatment_grid as 
							           SELECT * FROM crosstab(%L, %L) AS (plot_id text, block int, system_id int, treatment_id int, %s text)
							           order by treatment_id asc, plot_id asc',
							              row_query,
							              col_query,
							              cols);

	 EXECUTE sql_query;
END;
$$ language plpgsql;


/* 
select * from make_treatment_grid_view(2025);
select * from treatment_grid; 
*/