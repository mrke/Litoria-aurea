%% predict_my_pet
% Obtains predictions, using parameters and data

%%
function [Prd_data, info] = predict_Litoria_aurea(par, chem, T_ref, data)
  % created by Starrlight Augustine, Dina Lika, Bas Kooijman, Goncalo Marques and Laure Pecquerie 2015/01/30
  
  %% Syntax
  % [Prd_data, info] = <../predict_my_pet.m *predict_my_pet*>(par, chem, data)
  
  %% Description
  % Obtains predictions, using parameters and data
  %
  % Input
  %
  % * par: structure with parameters (see below)
  % * chem: structure with biochemical parameters
  % * data: structure with data (not all elements are used)
  %  
  % Output
  %
  % * Prd_data: structure with predicted values for data
  
  %% Remarks
  % Template for use in add_my_pet
  
  %% unpack par, chem, cpar and data
  cpar = parscomp_st(par, chem);
  v2struct(par); v2struct(chem); v2struct(cpar);
  v2struct(data);
  %k_J=k_M;
  pars_T=[T_A,T_L,T_H,T_AL,T_AH];
  %% compute temperature correction factors
  TC_ab = tempcorr(temp.ab, T_ref, pars_T);
  TC_ap = tempcorr(temp.ap, T_ref, pars_T);
  TC_am = tempcorr(temp.am, T_ref, pars_T);
  TC_Ri = tempcorr(temp.Ri, T_ref, pars_T);

  %% zero-variate data

  % life cycle
  pars_tp = [g; k; l_T; v_Hb; v_Hp];               % compose parameter vector
  [t_p, t_b, l_p, l_b, info] = get_tp(pars_tp, f); % -, scaled times & lengths at f
  
  % birth
  L_b = L_m * l_b;                  % cm, structural length at birth at f
  Lw_b = L_b/ del_M;                % cm, physical length at birth at f
  Wd_b = L_b^3 * d_V * (1 + f * w); % g, dry weight at birth at f (remove d_V for wet weight)
  aT_b = t_b/ k_M/ TC_ab;           % d, age at birth at f and T

  % puberty 
  L_p = L_m * l_p;                  % cm, structural length at puberty at f
  Lw_p = L_p/ del_M;                % cm, physical length at puberty at f
  Wd_p = L_p^3 * d_V * (1 + f * w); % g, dry weight at puberty (remove d_V for wet weight)
  aT_p = t_p/ k_M/ TC_ap;           % d, age at puberty at f and T

  % ultimate
  l_i = f - l_T;                    % -, scaled ultimate length at f
  L_i = L_m * l_i;                  % cm, ultimate structural length at f
  Lw_i = L_i/ del_M;                % cm, ultimate physical length at f
  Wd_i = L_i^3 * d_V * (1 + f * w); % g, ultimate dry weight (remove d_V for wet weight)
 
  % reproduction
  pars_R = [kap; kap_R; g; k_J; k_M; L_T; v; U_Hb; U_Hp]; % compose parameter vector at T
  RT_i = TC_Ri * reprod_rate(L_i, f, pars_R);             % #/d, ultimate reproduction rate at T

  % life span
  pars_tm = [g; l_T; h_a/ k_M^2; s_G];  % compose parameter vector at T_ref
  t_m = get_tm_s(pars_tm, f, l_b);      % -, scaled mean life span at T_ref
  aT_m = t_m/ k_M/ TC_am;               % d, mean life span at T
  
  %% pack to output
  % the names of the fields in the structure must be the same as the data names in the mydata file
  Prd_data.ab = aT_b;
  Prd_data.ap = aT_p;
  Prd_data.am = aT_m;
  Prd_data.Lb = Lw_b;
  Prd_data.Lp = Lw_p;
  Prd_data.Li = Lw_i;
  Prd_data.Wdb = Wd_b;
  Prd_data.Wdp = Wd_p;
  Prd_data.Wdi = Wd_i;
  Prd_data.Ri = RT_i;
  
%   %% uni-variate data
%   % time-length 
%   f = f_tL; pars_lb = [g; k; v_Hb];                          % compose parameters
%   ir_B = 3/ k_M + 3 * f * L_m/ v; r_B = 1/ ir_B;             % d, 1/von Bert growth rate
%   Lw_i = (f * L_m - L_T)/ del_M;                             % cm, ultimate physical length at f
%   Lw_b = get_lb(pars_lb, f) * L_m/ del_M;                    % cm, physical length at birth at f
%   EL = Lw_i - (Lw_i - Lw_b) * exp( - TC_tL * r_B * tL(:,1)); % cm, expected physical length at time
%   % length-weight
%   EW = (LW(:,1) * del_M).^3 * (1 + f * w);                   % g, expected wet weight at time
%   % O2-temperature
%     f = 1;
%   % yield coefficients  
%   y_E_X = kap_X * mu_X/ mu_E;      % mol/mol, yield of reserve on food
%   y_X_E = 1/ y_E_X;                % mol/mol, yield of food on reserve
%   y_V_E = mu_E * M_V/ E_G;         % mol/mol, yield of structure on reserve
%   y_P_X = kap_P * mu_X/ mu_P;    % mol/mol, yield of faeces on food 
%   y_P_E = y_P_X/ y_E_X;            % mol/mol, yield of faeces on reserve
%   % mass-power couplers
%   eta_XA = y_X_E/mu_E;             % mol/kJ, food-assim energy coupler
%   eta_PA = y_P_E/mu_E;             % mol/kJ, faeces-assim energy coupler
%   eta_VG = y_V_E/mu_E;             % mol/kJ, struct-growth energy coupler
%   eta_O = [-eta_XA  0       0;     % mol/kJ, mass-energy coupler
%     	   0        0       eta_VG;% used in: J_O = eta_O * p
% 	     1/mu_E  -1/mu_E   -1/mu_E;
%          eta_PA     0       0]; 
%   O2M = (- n_M\n_O)'; % -, matrix that converts organic to mineral fluxes  O2M is prepared for post-multiplication eq. 4.35
% 
%   p_ref = p_Am * L_m^2;               % J/d, max assimilation power at max size
%   pars_power = [kap; kap_R; g; k_J; k_M; L_T; v; U_Hb; U_Hp];
% 
%   L = (5.67/ d_V/ (1 + f * w)).^(1/3); % cm, structural length, use dry mass for 18.9 g wet mass
%   pACSJGRD = p_ref * scaled_power(L, f, pars_power, l_b, l_p);
%   pADG = pACSJGRD(:, [1 7 5]);  pADG(:,1) = 0; % exclude assim contribution
%   JM = pADG * eta_O' * O2M;                     % mol/d, mineral fluxes
%   %EO = - 24.4e3/ 24 * JM(:,3);                  % ml/h, O_2 consumption
%   %X_gas = (0.082058*(TO(:,1)+273.15))/(0.082058*293.15)*24.06;  % gas correction factor
%   X_gas = (0.082058*(20+273.15))/(0.082058*293.15)*24.06;  % gas correction factor
%   EO = (- 1 * (JM(:,3) * X_gas) .* tempcorr(273+TO(:,1), T_ref, pars_T))/18.9/24*1000;
% 
%   %% pack to output
%   % the names of the fields in the structure must be the same as the data names in the mydata file
%   Prd_data.tL = EL;
%   Prd_data.LW = EW;
%   Prd_data.TO = EO;