use super::Aura;
use codec::{Decode, Encode};
use frame_support::{
	decl_event, decl_module, decl_storage,
	dispatch::{DispatchResult, Vec},
	ensure,
};
#[cfg(feature = "std")]
use serde::{Deserialize, Serialize};
use sp_core::sr25519::{Public, Signature};
use sp_core::{H256, H512};
use sp_runtime::traits::{BlakeTwo256, Hash, SaturatedConversion};
use sp_runtime::transaction_validity::{TransactionLongevity, ValidTransaction};
use sp_std::collections::btree_map::BTreeMap;

// helps to inherit implementations and default capabilties from existing substrate framework. Gets access to a special type called `Events`

pub trait Trait: system::Trait {
	type Event: From<Event> + Into<<Self as system::Trait>::Event>;
}

pub type Value = u128;

// data types of an UTXO
#[cfg_attr(feature = "std", derive(Serialize, Deserialize))]
#[derive(PartialEq, Eq, PartialOrd, Ord, Default, Clone, Encode, Decode, Hash, Debug)]
pub struct TransactionInput {
	pub outpoint: H256,  // reference to a future UTXO to be spent
	pub sigscript: H512, // proof that entire transaction is untampered
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
	pub outputs: Vec<TransactionOutput>,
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


		/// Total reward value to be redistributed among the authorities
		pub RewardTotal get(fn reward_total): Value;
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
			let reward = Self::validate_transaction(&transaction)?;

			// 2. Write to storage
			Self::update_storage(&transaction, reward)?;

			// 3. Emit event
			Self::deposit_event(Event::TransactionSuccess(transaction));
			Ok(())
		}

		// custom substrate function. Executed by the runtime at the end of every block
		fn on_finalize() {
			// retrieve the block of validators
			// retrieves the public key and convert into a datatype before collecting into a typed vector
			let auth: Vec<_> = Aura::authorities().iter().map(|x| {
				let r: &Public = x.as_ref();
				r.0.into()
			}).collect();
			Self::disperse_reward(&auth);
		}

	}
}

decl_event! {
	pub enum Event {
		TransactionSuccess(Transaction),
	}
}

impl<T: Trait> Module<T> {


	pub fn get_simple_transaction(transaction: &Transaction) -> Vec<u8> {

		let mut trx = transaction.clone();
		for input in trx.inputs.iter_mut() {
			input.sigscript = H512::zero(); //0x0000000000000000
		}
		trx.encode()

	}

	pub fn validate_transaction(transaction: &Transaction) -> Result<Value, &'static str> {
		ensure!( transaction.inputs.is_empty(), "No inputs");
		ensure!( transaction.outputs.is_empty(), "No outputs");

		{
			let input_set: BTreeMap<_, ()> = transaction.inputs.iter().map(|input| (input, ())).collect();
			ensure!(input_set.len() == transaction.inputs.len(), "each input must only be used once");
		}

		{
			let output_set: BTreeMap<_, ()> = transaction.outputs.iter().map(|output| (output, ())).collect();
			ensure!(output_set.len() == transaction.outputs.len(), "each output must only be used once");
		}

		let simple_transaction = Self::get_simple_transaction(transaction);
		let mut total_input : Value = 0;
		let mut total_output : Value = 0;

		for input in transaction.inputs.iter() {
			if let Some(input_utxo) = <UtxoStore>::get(&input.outpoint) {
				ensure!(
					sp_io::crypto::sr25519_verify(
						&Signature::from_raw(
							*input.sigscript.as_fixed_bytes()),
							&simple_transaction,
							&Public::from_h256(input_utxo.pubkey)
						), "signature must be valid" );

				total_input = total_input.checked_add(input_utxo.value).ok_or("input value overflow")?;


			}	else {
				// todo: Handle race conditions

			}
		}

		let mut output_index: u64 = 0;
		for output in transaction.outputs.iter() {
			ensure!(output.value > 0, "output value must be non-zero");
			let hash = BlakeTwo256::hash_of(&(&transaction.encode(), output_index));
			output_index = output_index.checked_add(1).ok_or("output index overflow")?;
			ensure!(! <UtxoStore>::contains_key(hash), "output already exists");
			total_output = total_output.checked_add(output.value).ok_or("Output value has overflowed.")?;
		}

		ensure!( total_input >= total_output, "output value must not exceed the input value");
		let reward = total_input.checked_sub(total_output).ok_or("reward underflow")?;

		Ok(reward)
	}

	fn update_storage(transaction: &Transaction, reward: Value) -> DispatchResult {
		let new_total = <RewardTotal>::get()
			.checked_add(reward)
			.ok_or("reward overflow")?;
		<RewardTotal>::put(new_total);

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

	fn disperse_reward(authorities: &[H256]) {
		// divide reward fairly

		let reward = <RewardTotal>::take();
		let share_value: Value = reward
			.checked_div(authorities.len() as Value)
			.ok_or("No authorities...")
			.unwrap();

		if share_value == 0 {
			return;
		}

		let remainder = reward
			.checked_sub(share_value * authorities.len() as Value)
			.ok_or("sub underflow")
			.unwrap();

		<RewardTotal>::put(remainder as Value);
		// create utxo per validator

		for authority in authorities {
			let utxo = TransactionOutput {
				value: share_value,
				pubkey: *authority,
			};

			let hash = BlakeTwo256::hash_of(&(
				&utxo,
				<system::Module<T>>::block_number().saturated_into::<u64>(),
			));

			if !<UtxoStore>::contains_key(hash) {
				<UtxoStore>::insert(hash, utxo);
				sp_runtime::print("transaction reward sent to");
				sp_runtime::print(hash.as_fixed_bytes() as &[u8]);
			} else {
				sp_runtime::print("Transaction reward wasted due to a hash collision");
			}
		}

		// write the new UTXOs into utxo stores
	}
}

/// Tests for this module
#[cfg(test)]
mod tests {
	use super::*;

	use frame_support::{assert_err, assert_ok, impl_outer_origin, parameter_types, weights::Weight};
	use sp_core::testing::{KeyStore, SR25519};
	use sp_core::traits::KeystoreExt;
	use sp_runtime::{testing::Header, traits::IdentityLookup, Perbill};

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
