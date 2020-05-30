use super::Aura;
use codec::{Decode, Encode};
use frame_support::{
	decl_event, decl_module, decl_storage,
	dispatch::{DispatchResult, Vec},
	ensure,
};
use sp_core::{H256, H512};
#[cfg(feature = "std")]
use serde::{Deserialize, Serialize};
use sp_core::sr25519::{Public, Signature};
use sp_runtime::traits::{BlakeTwo256, Hash, SaturatedConversion};
use sp_std::collections::btree_map::BTreeMap;
use sp_runtime::transaction_validity::{TransactionLongevity, ValidTransaction};

// helps to inherit implementations and default capabilties from existing substrate framework. Gets access to a special type called `Events`

pub trait Trait: system::Trait {
	type Event: From<Event> + Into<<Self as system::Trait>::Event>;
}

pub type Value = u128;

// data types of an UTXO
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Default, Clone, Encode, Decode, Hash, Debug)]
pub struct TransactionInput {
	pub outpoint: H256 , // reference to a future UTXO to be spent
	pub sigscript: H512  , // proof that entire transaction is untampered
}

#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Default, Clone, Encode, Decode, Hash, Debug)]
pub struct TransactionOutput {
	pub value: Value, // value associated with this UTXO
	pub pubkey: H256, // public key associated with this output
}

#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Default, Clone, Encode, Decode, Hash, Debug)]
pub struct Transaction {
	pub inputs: Vec<TransactionInput>,
	pub outputs: Vec<TransactionOutput>
}

// Main developer interface.
// name of the macro
decl_storage! {

	// custom substrate syntax
	// Build a custom hashmaps based on a closure 
	trait Store for Module<T: Trait> as Utxo {
		UtxoStore build(|config: &GenesisConfig| {
			config.genesis_utxos
			.iter()
			.cloned()
			.map(|u| (BlakeTwo256::hash_of(&u), u) )
			.collect::<Vec<_>>()
		}): map hasher(identity) H256 => Option<TransactionOutput>;
	}
	
	// bootstrap some values 
	add_extra_genesis {
		config(genesis_utxos): Vec<TransactionOutput>;
	} 
}

// External functions: callable by the end user
decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event() = default;
		

		pub fn spend(_origin, transaction: Transaction) -> DispatchResult {
			
			// 1. Check transaction is valid 
			
			// 2. Write to storage
			Self::update_storage(&transaction)?;

			// 3. Emit event
			
			Ok(())
		}

	}
}

decl_event! {
	pub enum Event {

	}
}

impl<T: Trait> Module<T> {

	fn update_storage(transaction: &Transaction) -> DispatchResult {
		// 1. Remove the input UTOX from UTXOStore
		for input in &transaction.inputs {
			<UtxoStore>::remove(input.outpoint);
		}

		// 2. Create the new UTXOs in UTXO stored
		let mut index: u64 = 0;
		for output in &transaction.outputs {

			// calculate the hash out the outputs
			let hash = BlakeTwo256::hash_of(&(&transaction.encode(), index));

			index = index.checked_add(1).ok_or("output index overflow")?;
			// 50, 0x000
			<UtxoStore>::insert(hash, output);

		}

		Ok(())
	}
}


/// Tests for this module
#[cfg(test)]
mod tests {
	use super::*;

	use frame_support::{assert_ok, assert_err, impl_outer_origin, parameter_types, weights::Weight};
	use sp_runtime::{testing::Header, traits::IdentityLookup, Perbill};
	use sp_core::testing::{KeyStore, SR25519};
	use sp_core::traits::KeystoreExt;

	impl_outer_origin! {
		pub enum Origin for Test {}
	}

	#[derive(Clone, Eq, PartialEq)]
	pub struct Test;
	parameter_types! {
			pub const BlockHashCount: u64 = 250;
			pub const MaximumBlockWeight: Weight = 1024;
			pub const MaximumBlockLength: u32 = 2 * 1024;
			pub const AvailableBlockRatio: Perbill = Perbill::from_percent(75);
	}
	impl system::Trait for Test {
		type Origin = Origin;
		type Call = ();
		type Index = u64;
		type BlockNumber = u64;
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type AccountId = u64;
		type Lookup = IdentityLookup<Self::AccountId>;
		type Header = Header;
		type Event = ();
		type BlockHashCount = BlockHashCount;
		type MaximumBlockWeight = MaximumBlockWeight;
		type MaximumBlockLength = MaximumBlockLength;
		type AvailableBlockRatio = AvailableBlockRatio;
		type Version = ();
		type ModuleToIndex = ();
		type AccountData = ();
		type OnNewAccount = ();
		type OnKilledAccount = ();
	}
	impl Trait for Test {
		type Event = ();
	}

	type Utxo = Module<Test>;

}
