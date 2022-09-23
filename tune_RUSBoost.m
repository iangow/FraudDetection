%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tune hyperparameter (number of trees) for RUSBoost model              %
% Our results are generated using Matlab R2020b on Windows 10           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% training period: 1991-1999, validating period: 2001
% evaluation metric: AUC
diary("tune_rusboost.txt");
year_valid = 2001; 
for iters = [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1500, 2000, 2500, 3000] % parameter space to search
    rng(0,'twister'); % fix random seed for reproducing the results
    % read training data
    fprintf('==> Validating RUSBoost-iters%d (training period: %d-%d, validating period: %d, with %d-year gap)...\n',iters, 1991, year_valid-2, year_valid, 2);
    data_train = data_reader('data_FraudDetection_JAR2020.csv', 'data_default', 1991, year_valid-2);
    y_train = data_train.labels;
    X_train = data_train.features;
    paaer_train = data_train.paaers;
    % read validating data
    data_valid = data_reader('data_FraudDetection_JAR2020.csv', 'data_default', year_valid, year_valid);
    y_valid = data_valid.labels;
    X_valid = data_valid.features;
    paaer_valid = unique(data_valid.paaers(data_valid.labels~=0));
    % handle serial frauds using PAAER
    y_train(ismember(paaer_train,paaer_valid)) = 0;
    % train model
    t = templateTree('MinLeafSize',5); % base model
    rusboost = fitensemble(X_train,y_train,'RUSBoost',iters,t,'LearnRate',0.1,'RatioToSmallest',[1 1]);
    % validate model
    [label_predict,dec_values] = predict(rusboost,X_valid);
    dec_values = dec_values(:,2);
    % print validation results
    metrics = evaluate(y_valid,label_predict,dec_values,0.01);
    fprintf('Number of Iterations/Trees: %d ==> AUC: %.4f \n', iters, metrics.auc);
end
diary off;
