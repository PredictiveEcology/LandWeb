checkoutVersion <- function(gitHash, githubPATname = "GITHUB_PAT",
                httpsURL="https://github.com/eliotmcintire/LandWeb.git",
                sshURL="git@github.com:eliotmcintire/LandWeb.git") {

  cred <- git2r::cred_token("GITHUB_PAT")
  repo <- git2r::init(".")
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
  git2r::checkout(lookup(repo, LandWebVersion))
  return(repo)
}