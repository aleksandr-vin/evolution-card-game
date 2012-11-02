-ifndef(evocg_include_cards_hrl).
-define(evocg_include_cards_hrl, 1).
-define(mandatory_field(Name), Name = throw({error, 'mandatory-field'})).



-record(card,{?mandatory_field(prop),prop_2}). % card properties


% prop_types

-define(prp_carnivouros, prp_carnivouros). % хищник
-define(prp_poisonous, prp_poisonous). % ядовитое
-define(prp_running, prp_running). % быстрое
-define(prp_high_body_weight, prp_high_body_weight). % большой
-define(prp_fat_tissue, prp_fat_tissue). % жировой запас
-define(prp_swimming, prp_swimming). % водоплавающее
-define(prp_burrowing, prp_burrowing). % норное
-define(prp_tail_loss, prp_tail_loss). % отбрасывание хвоста
-define(prp_piracy, prp_piracy). % пиратство
-define(prp_hibernation_ability, prp_hibernation_ability). % спячка
-define(prp_sharp_vision, prp_sharp_vision). % острое зрение
-define(prp_camouflage, prp_camouflage). % камуфляж
-define(prp_mimicry, prp_mimicry). % мимикрия
-define(prp_scavanger, prp_scavanger). % падальщик
-define(prp_grazing, prp_grazing). % топотун
-define(prp_symbiosus, prp_symbiosus). % симбиоз
-define(prp_parasite, prp_parasite). % паразит
-define(prp_cooperation, prp_cooperation). % сотрудничество
-define(prp_communication, prp_communication). % взаимодействие

 -endif.