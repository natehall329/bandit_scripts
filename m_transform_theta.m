function [theta_trans] = m_transform_theta(theta, inF)

theta_trans = VBA_sigmoid(theta) ; %three-param model all have sigmoid transformations

% alpha_win = 1./(1+exp(-theta(1))); % learning rate is bounded between 0 and 1.
%     alpha_loss = 1./(1+exp(-theta(2))); % learning rate is bounded between 0 and 1.
%     decay = 1./(1+exp(-theta(3))); % decay is bounded between 0 and 1.

end