import sys

sys.path.append(".")
import deploy
from unittest import mock


class TestDeployStandardDir(object):
    def test_creates_symlink(self, tmp_path):
        deployer = deploy.Deployer(dry_run=False, force=False)

        deployer.deploy_standard_dir("vim", install_path=tmp_path)

        assert tmp_path.joinpath(".vim").exists()

    def test_does_not_overwrite(self, tmp_path):
        tmp_path.joinpath(".vim").touch()
        deployer = deploy.Deployer(dry_run=False, force=False)

        with mock.patch("deploy.Path.symlink_to") as mock_symlink:
            deployer.deploy_standard_dir("vim", install_path=tmp_path)

        mock_symlink.assert_not_called()

    def test_overwrites_with_force(self, tmp_path):
        tmp_path.joinpath(".vim").touch()
        deployer = deploy.Deployer(dry_run=False, force=True)

        with mock.patch("deploy.Path.symlink_to") as mock_symlink:
            deployer.deploy_standard_dir("vim", install_path=tmp_path)

        mock_symlink.assert_called_once()
