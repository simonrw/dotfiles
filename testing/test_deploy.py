import sys

sys.path.append(".")
import deploy
from unittest import mock


def test_creates_symlink(tmp_path):
    args = create_args()
    deployer = deploy.Deployer(args)

    deployer.deploy_standard_dir("vim", install_path=tmp_path)

    assert tmp_path.joinpath(".vim").exists()


def test_does_not_overwrite(tmp_path):
    tmp_path.joinpath(".vim").touch()
    args = create_args()
    deployer = deploy.Deployer(args)

    with mock.patch("deploy.Path.symlink_to") as mock_symlink:
        deployer.deploy_standard_dir("vim", install_path=tmp_path)

    mock_symlink.assert_not_called()


def test_overwrites_with_force(tmp_path):
    tmp_path.joinpath(".vim").touch()
    args = create_args(force=True)
    deployer = deploy.Deployer(args)

    with mock.patch("deploy.Path.symlink_to") as mock_symlink:
        deployer.deploy_standard_dir("vim", install_path=tmp_path)

    mock_symlink.assert_called_once()


# helpers


def create_args(**kwargs):
    """Returns default arguments for usage.

    Can have overrides.
    """

    args = mock.Mock()

    defaults = {"dry_run": False, "force": False}

    for key in defaults:
        setattr(args, key, defaults[key])

    for key in kwargs:
        setattr(args, key, kwargs[key])

    return args
