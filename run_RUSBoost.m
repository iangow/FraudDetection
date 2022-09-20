%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% run RUSBoost model with 28 raw accounting variables as features %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% set parameters
iters = 3000; % the number of iterations/trees of RUSBoost model
gap = 2; % the gap between training and testing periods, 2-year gap by default

%% train and test models
for year_test = 2003:2008
    rng(0,'twister'); % fix random seed for reproducing the results

    % read training data
    fprintf('Running RUSBoost (training period: 1991-%d, testing period: %d, with %d-year gap)...\n',year_test-gap,year_test,gap);
    data_train = data_reader('data_FraudDetection_JAR2020.csv','data_default',1991,year_test-gap);
    y_train = data_train.labels;
    X_train = data_train.features;
    paaer_train = data_train.paaers;

    % read testing data
    data_test = data_reader('data_FraudDetection_JAR2020.csv','data_default',year_test,year_test);
    y_test = data_test.labels;
    X_test = data_test.features;
    paaer_test = unique(data_test.paaers(data_test.labels~=0));

    % handle serial frauds using PAAER
    y_train(ismember(paaer_train,paaer_test)) = 0;

    % train model
    t1 = tic;
    t = templateTree('MinLeafSize',5); % base model
    % fit RUSBoost model (default parameters: learning rate: 0.1, RatioToSmallest: [1 1])
    rusboost = fitensemble(X_train,y_train,'RUSBoost',iters,t,'LearnRate',0.1,'RatioToSmallest',[1 1]);
    t_train = toc(t1);
    % turn on the following line of code if you want to get feature importance
    % [imp,ma] = predictorImportance(rusboost);

    % test model
    t2 = tic;
    [label_predict,dec_values] = predict(rusboost,X_test); % predict frauds in the testing year
    dec_values = dec_values(:,2); % get fraud probability
    t_test = toc(t2);

    % print evaluation results
    fprintf('Training time: %g seconds | Testing time %g seconds \n', t_train, t_test);
    metrics = evaluate(y_test,label_predict,dec_values,0.01); % topN=0.01
    fprintf('Performance (top1%% as cut-off thresh): \n');
    fprintf('AUC: %.4f \n', metrics.auc);
    fprintf('NCDG@k=top1%%: %.4f \n', metrics.ndcg_at_k);
    fprintf('Sensitivity: %.2f%% \n', metrics.sensitivity_topk*100);
    fprintf('Precision: %.2f%% \n', metrics.precision_topk*100);
    % fprintf('Importance of predictors:%d\n', output.imp);

    % turn on the following lines of code if your want to save prediction results in a file
    output_filename = ['prediction_rusboost28_',num2str(year_test),'.csv'];
    dlmwrite(output_filename,[data_test.years, data_test.firms, y_test, dec_values],'precision','%g');

end
