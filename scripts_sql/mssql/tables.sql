-- harvestings
CREATE TABLE WEIR.wicst.harvestings (
	harvesting_id varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	yielding_type varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	harvest_date datetime NULL,
	plot_id varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
	product varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	product_description varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	harvest_area float NULL,
	percent_moisture float NULL,
	harvest_lbs float NULL,
	CONSTRAINT harvestings_pk PRIMARY KEY (harvesting_id)
	CONSTRAINT harvestings_product_check CHECK (product IN (
		'corn',
		'soybean',
		'wheat grain',
		'wheat straw',
		'wheatlage',
		'alfalfa',
		'oat grain',
		'oat straw',
		'oatlage',
		'pasture'
	))
);