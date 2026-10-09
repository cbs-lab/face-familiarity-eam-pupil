% Source - https://stackoverflow.com/a
% Posted by Rafael Monteiro, modified by community. See post 'Timeline' for change history
% Retrieved 2026-01-14, License - CC BY-SA 3.0

inputFolder = '/Users/rfournier/Documents/PhD_Exp/WP3/Test_picture/Ori/';
outputFolder = '/Users/rfournier/Documents/PhD_Exp/WP3/Test_picture/Scrambled/';

% Create output folder if it doesn't exist
if ~exist(outputFolder, 'dir')
    mkdir(outputFolder);
end

% Get all image files in the input folder (adjust extensions as needed)
imageFiles = dir(fullfile(inputFolder, '*.jpg'));
% If you have other formats, you can add them:
% imageFiles = [dir(fullfile(inputFolder, '*.jpg')); dir(fullfile(inputFolder, '*.png'))];

% Scrambling parameters
blockSize_h = 45;
blockSize_w = 45;

% Loop through all images
for i = 1:length(imageFiles)
    % Read the image
    filename = imageFiles(i).name;
    img = imread(fullfile(inputFolder, filename));
    
    % Calculate number of blocks
    nRows = size(img, 1) / blockSize_w;
    nCols = size(img, 2) / blockSize_h;
    
    % Scramble the image
    scramble = mat2cell(img, ones(1, nRows) * blockSize_w, ones(1, nCols) * blockSize_h, size(img, 3));
    scramble = cell2mat(reshape(scramble(randperm(nRows * nCols)), nRows, nCols));
    
    % Save scrambled image
    [~, name, ext] = fileparts(filename);
    outputFilename = fullfile(outputFolder, [name '_scrambled' ext]);
    imwrite(scramble, outputFilename);
    
    % Optional: display progress
    fprintf('Processed %d/%d: %s\n', i, length(imageFiles), filename);
end

fprintf('All images scrambled successfully!\n');