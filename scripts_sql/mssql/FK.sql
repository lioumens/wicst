-- WEIR.wicst.harvestings foreign keys
ALTER TABLE WEIR.wicst.harvestings ADD CONSTRAINT harvestings_plots_FK FOREIGN KEY (plot_id) REFERENCES WEIR.wicst.plots(plot_id);
ALTER TABLE WEIR.wicst.harvestings ADD CONSTRAINT harvestings_yieldings_FK FOREIGN KEY (harvesting_id) REFERENCES WEIR.wicst.yieldings(yielding_id) ON DELETE CASCADE ON UPDATE CASCADE;

-- WEIR.wicst.harvestingdetails foreign keys
ALTER TABLE WEIR.wicst.harvestingdetails ADD CONSTRAINT harvestingdetails_harvestings_FK FOREIGN KEY (harvesting_id) REFERENCES WEIR.wicst.harvestings(harvesting_id) ON DELETE CASCADE ON UPDATE CASCADE;

-- WEIR.wicst.biomassings foreign keys
ALTER TABLE WEIR.wicst.biomassings ADD CONSTRAINT biomassings_plots_FK FOREIGN KEY (plot_id) REFERENCES WEIR.wicst.plots(plot_id);
ALTER TABLE WEIR.wicst.biomassings ADD CONSTRAINT biomassings_yieldings_FK FOREIGN KEY (biomassing_id) REFERENCES WEIR.wicst.yieldings(yielding_id) ON DELETE CASCADE ON UPDATE CASCADE;

-- WEIR.wicst.biomassingdetails foreign keys
ALTER TABLE WEIR.wicst.biomassingdetails ADD CONSTRAINT biomassingdetails_biomassings_FK FOREIGN KEY (biomassing_id) REFERENCES WEIR.wicst.biomassings(biomassing_id) ON DELETE CASCADE ON UPDATE CASCADE;

-- WEIR.wicst.losses foreign keys
ALTER TABLE WEIR.wicst.losses ADD CONSTRAINT losses_harvestings_FK FOREIGN KEY (harvesting_id) REFERENCES WEIR.wicst.harvestings(harvesting_id) ON DELETE CASCADE ON UPDATE CASCADE;