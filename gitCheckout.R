checkoutVersion <- function(gitHash, localRepoPath=".", githubPATname = "GITHUB_PAT",
                            httpsURL="https://github.com/eliotmcintire/LandWeb.git",
                            sshURL="git@github.com:eliotmcintire/LandWeb.git") {
  
  cred <- git2r::cred_token("GITHUB_PAT")
  repo <- git2r::init(localRepoPath)
  #httpsURL <- "https://github.com/eliotmcintire/LandWeb.git"
  #sshURL <- "git@github.com:eliotmcintire/LandWeb.git"
  remoteWasHTTPS <- git2r::remote_url(repo)==httpsURL
  if(!remoteWasHTTPS)
    git2r::remote_set_url(repo, "origin", url=httpsURL)
  
  # Get specific LandWeb version
  hasUncommittedFiles <- sum(sapply(status(repo), length))>0
  if(hasUncommittedFiles) {
    lastCommit <- revparse_single(repo, "HEAD")
    git2r::add(repo, unlist(status(repo)$unstaged))
    tempCommit <- commit(repo, "testing")
  }
  git2r::checkout(lookup(repo, gitHash))
  return(list(repo=repo, hasUncommittedFiles=hasUncommittedFiles, lastCommit=lastCommit, 
              remoteWasHTTPS=remoteWasHTTPS, sshURL=sshURL))
}

checkoutDev <- function(checkoutCondition) {
  checkout(checkoutCondition$repo, "development")
  if(checkoutCondition$hasUncommittedFiles) git2r::reset(checkoutCondition$lastCommit, 
                                                         reset_type = "soft")
  if(!checkoutCondition$remoteWasHTTPS)
    remote_set_url(checkoutCondition$repo, "origin", url=checkoutCondition$sshURL)
  
}