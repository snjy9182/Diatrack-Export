function [] = ExportTrajDia()
    directory_name = '/Users/sunjayyoo/Dropbox/Work/Diatrack\ Export/';
    fileName = 'HTZ1Halo_fr10ms_120mW_14.mat';  % file name 
    load('HTZ1Halo_fr10ms_120mW_14.mat', 'tracks', 'FrameLength','lastFrameSub');  % load in variable 'tracks' & 'frameLength' from DiaTracks .mat file
    %****FOR PARTICLES FOUND INITIALLY IN FRAME #1****
    for i = 1:length(tracks{1}.Successor);  % loop for all particles found in frame #1
        coords = zeros(lastFrameSub,3);  % start with an empty matrix to place coordinates (XYZ)
        coords(1,:) = round([tracks{1}.RefinedCooX(i),tracks{1}.RefinedCooY(i),tracks{1}.RefinedCooZ(i)]*100)/100;  % round coords to 2 decimals
        if tracks{1}.Successor(i) == 0;  % if particles do not have successor in frame #1
            cellCoords{i} = coords;  %write coords to cell.  Each cell is a single particle track
        else
            current_num = tracks{1}.Successor(i);  % current particle to monitor
            m = 2;  % start initial frame counter
            while current_num > 0 && m <= FrameLength;
                coords(m,:) = round([tracks{m}.RefinedCooX(current_num),tracks{m}.RefinedCooY(current_num),tracks{m}.RefinedCooZ(current_num)]*100)/100;  % round coords to 2 decimals
                current_num = tracks{m}.Successor(current_num);  % does current particle have a successor?
                m = m+1;  % loop frame counter
            end
            if nnz(coords(:,1)) >= 2  % make sure tracks are at least 2 time points
                cellCoords{i} = coords;  % write coords to cell.  Each cell is a single particle track
            end
        end
    end
    %****FOR PARTICLES FOUND INITIALLY IN FRAME >#1****
    for j = 1:FrameLength-1  % loop for all additional particles not intially found in frame #1
        current_values = sort(transpose(nonzeros(tracks{j}.Successor)));  % previous particles that have already been identified
        all_values = 1:length(tracks{j+1}.Successor);  % array of all particles in frame
        new_values = setxor(current_values, all_values);  % new particles that appear in frame
        for n = 1:length(new_values)  % loop for all new particles
            coords = zeros(lastFrameSub,3);
            coords(j+1,:) = round([tracks{j+1}.RefinedCooX(n),tracks{j+1}.RefinedCooY(n),tracks{j+1}.RefinedCooZ(n)]*100)/100;  % round coords to 2 decimals
            if new_values(1,n) == 0  % if new particles do not have successor
                cellCoords{length(cellCoords)+1} = coords;  % write coords to cell.  Each cell is a single particle track
            else
                current_num = new_values(1,n);  % current particle to monitor
                m = j+1;  % loop frame counter
                while current_num > 0 && m <= FrameLength
                    coords(m,:) = round([tracks{m}.RefinedCooX(current_num),tracks{m}.RefinedCooY(current_num),tracks{m}.RefinedCooZ(current_num)]*100)/100;  % round coords to 2 decimals
                    current_num = tracks{m}.Successor(1,current_num);  % does current particle have a successor?
                    m = m+1;  % loop frame counter
                end
                if nnz(coords(:,1)) >= 2  % make sure tracks are at least 2 time points
                    cellCoords{length(cellCoords)+1} = coords;  % write coords to cell.  Each cell is a single particle track
                end
            end
        end
    end
    %****MANIPULATE TO DIATRACK FORMAT****
    firstFrame = zeros(1,length(cellCoords)*3);  % empty matrix preallocated for speed
    mat = zeros(FrameLength,length(cellCoords)*3);  % empty matric preallocated for speed
    for i = 1:length(cellCoords);  % loop for number of particle tracks
        firstFrame(:,3*i-2:3*i) = [find(cellCoords{i},1,'first'),0,0];  % first frame that particle appears
        data = cellCoords{i};
        data( ~any(data,2), : ) = [];  % remove empty rows
        data( :, ~any(data,1) ) = [];  % remove empty columns
        cellCoords{i} = data;  % tracks
        coords = zeros(lastFrameSub,3); % correct size for matrix
        [r,c] = size(cellCoords{i});  % find indicies for inserting coords into empty matrix
        coords(1:r,1:c) = cellCoords{i};  % insert coords into proper place
        mat(:,3*i-2:3*i) = coords;  % concatenate additional tracks
    end
    %****WRITE TO .TXT FILE****
    fileID = fopen('test.txt','w');
    fprintf(fileID,'format (columnwise): Frame1 row n+1: (y(tn) x(tn) z(tn)), row n+1: (y(t(n+1)) x(t(n+1)) z(t(n+1))), row n+2: (y(t(n+2)) x(t(n+2) z(t(n+2)) y(t(n+3)).... where Frame1 is the frame number where the target is seen for the first time, and the columns define trajectories. Beware! the number of tracks is limited by the width of the widest text file on your machine. Rowwise export preferred \t');
    fprintf(fileID,'\r');
    dlmwrite('test.txt',vertcat(firstFrame,mat),'-append','delimiter','\t')
    fclose(fileID);
    clearvars -except directory_name files fileIndex formatSpec