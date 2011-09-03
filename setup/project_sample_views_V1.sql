create or replace view sample_view_data_norm_V1 as
select
  items.id,
  items.KEY1,
  items.KEY2,
  items.KEY3,
  data_norm.PERIOD,
  data_norm.V
from
  sample_items items left join 
  sample_data_norm_V1 data_norm on (items.id = data_norm.item_id)
order by
  items.id,
  data_norm.PERIOD
;

create or replace view sample_view_summary_V1 as select
  items.KEY1,
  items.KEY2,
  items.KEY3,
  sm.*
from
  sample_items items left join
  sample_summary_V1 s on (items.id = s.id) left join
  sample_summary_models_V1 sm on (items.id = sm.item_id)
where
  s.BestModel = sm.model
order by
  items.id
;

create or replace view sample_view_results_V1 as select
  items.id,
  items.KEY1,
  items.KEY2,
  items.KEY3,
  r.PERIOD,
  r.V
from
  sample_items items left join
  sample_summary_V1 s on (items.id=s.id) left join
  sample_results_V1 r on (items.id=r.item_id)
where
  s.BestModel = r.model
order by
  items.id,
  r.PERIOD
;

